(require 'org)
(require 'org-clock)
(require 'subr-x)                       ; when-let, thread-first
(require 'map)                          ; map-keys

;;;; ---------------
;;;; form processing
;;;; ---------------

(defvar org-catch--helpers-prefix "org-catch--helper-"
  "Prefix for helper functions that intended to be used in `org-catch' DATA argument. This prefix can be omitted in the DATA forms (values for keys) for readability. Mind, however, possible namespace clashes.")

(defun org-catch--preprocess-form (form &optional pos collected-pairs)
  "Recursively preprocess the FORM or if FORM is a list its element at the POS position when called recursively. COLLECTED-PAIRS is plist of previously collected values for keys that can be used for substitution if FORM has ':symbol' pattern.

The following substitution rules are applied:

- if the FORM is a list of alists (no longer than 4 elements) then select element according to universal prefix. The car of each alist is universal prefix value and cdr is body to preprocess and evaluate. See `org-catch--record'.
- if FORM is a list then each element is preprocessed recursively
- if FORM is a symbol and matches a prefixed helper function (see `org-catch--helpers-prefix') it is substituted with this function, i.e., `region' -> `org-catch--region'. Also if the FORM matches a prefixed helper function and is not a first symbol in a list (controlled with POS arg) it will be wrapped in a funcall, i.e. region -> (org-catch--region)
- if FORM is a symbol that starts with ':' and matches to the corresponding key's value if it was already evaluated it will be substituted, e.g., :item -> 'New item' (assuming :item key's value was already set to 'New item' earlier)
- in any other case the FORM is left as it is
- at the end the preprocessed FORM is evaluated and should return the value for template's key (see `org-catch--record')

Returns preprocessed form."
  (pcase form
    ('nil nil)
    ((pred stringp) form)
    ((pred symbolp) (cond
                     ;; substitute with previously collected values
                     ((and (string-prefix-p ":" (symbol-name form))
                           (plist-member collected-pairs form))
                      (plist-get collected-pairs form))
                     ;; substitute with org-catch-- functions
                     ((when-let
                          ((form (symbol-name form))
                           (form (concat org-catch--helpers-prefix form))
                           (fun-symbol (intern form))
                           ;; check if function exists
                           ((functionp fun-symbol)))
                        ;; wrap symbol in parenthesis as a 'funcall' if not fist in list
                        (if (and pos (= pos 0))
                            fun-symbol
                          (list fun-symbol))))
                     (t form)))
    ((pred listp) (seq-map-indexed
                   (lambda (f p)
                     (org-catch--preprocess-form f p collected-pairs))
                   form))))

;; (org-catch--preprocess-form '(:item region) nil '(:item "Hello"))
;; (org-catch--preprocess-form :item nil '(:item "Hello"))
;; (org-catch--preprocess-form 'region)
;; (org-catch--preprocess-form '(link :item) nil '(:item "Hello"))

(defun org-catch--template-is-ok-p (template)
  "Check if TEMPLATE is valid"
  (if-let (((plistp template))
           ;; (keys (map-keys template))
           ;; check if required fields are present
           ;; ((member :item keys))
           )
      t
    (error "org-catch -- TEMPLATE is misformed. Shuld be plist.")))

(defvar org-catch---key-prompt nil
  "For storing user prompt generated from template's key.")

(defun org-catch--record (template &optional arg)
  "Collects data for TEMPLATE. The TEMPLATE is plist with keys as symbols like `:key' and value forms that are preprocessed with `org-catch--preprocess-form' (see docs for form specification) and evaluated at point where `org-catch' is called."
  (when-let (((org-catch--template-is-ok-p template))
             (pairs (map-pairs template)))
    (let (collected-pairs)
      (mapcar
       (pcase-lambda (`(,key . ,val))
         ;; check if we need to eval first (backquote form)
         (when (equal (car-safe val) backquote-backquote-symbol)
           (setq val (eval val)))
         ;; check alternative specs for universal argument
         (when-let (((listp val))
                    ((seq-every-p 'listp val))
                    ((seq-every-p (lambda (v) (member (car v) '(1 4 16 64))) val))
                    ;; convert universal arg from "P" to "p" variant
                    (arg (pcase arg
                           ((or 1 'nil)   1)
                           ((or 4 '(4))   4)
                           ((or 16 '(16)) 16)
                           ((or 64 '(64)) 64)
                           (_ (error "ARG (universal) argument should be either a number or raw (but no more than 3 times: C-u C-u C-u)")))))
           (setq val (cdr-safe (assoc arg val))))
         ;; get val
         (setq org-catch---key-prompt
               (thread-first            ; subr-x provides
                 (symbol-name key)
                 (substring 1)
                 (capitalize)
                 (concat ": ")))
         (setq val
               (org-with-wide-buffer
                (eval (org-catch--preprocess-form val nil collected-pairs))))
         (setq org-catch---key-prompt nil)
         (setq collected-pairs (plist-put collected-pairs key val)))
       pairs)
      collected-pairs)))

;; (org-catch--record '(:key ((1 . "a")
;;                            (4 . "b")
;;                            (16 . "c"))) '(4))

;; (org-catch--record '(:key ((64 . "x")
;;                            (16 . "c"))) 64)

;; (org-catch--record '(:key ((64 . "x")
;;                            (16 . "c"))) nil)

;; expect error
;; (org-catch--record '(:key ((65 . "x"))))

;;;; ------------------------------
;;;; keywords processing and filing
;;;; ------------------------------

;;; processing special keys

(defun org-catch--process-target (data)
  "Process :target form. Check type and if string (org link) translate it to buffer or marker."
  (let ((target (plist-get data :target))
        (m (make-marker)))
    (pcase target
      ;; valid types no processing
      ((or 'nil
           (pred markerp)
           (pred bufferp))
       target)
      ;; string is assumed to be org-link
      ;; - just org file link (return buffer)
      ((and (pred stringp)
            (pred (lambda (s) (string-match-p "\\.org\\'" s))))
       (or (find-file-noselect (expand-file-name target))
           (error "Can not find org file target: " target)))
      ;; - org file with search element (return marker)
      ((and (pred stringp)
            (pred (lambda (s) (string-match-p "\\.org::" s))))
       (save-window-excursion
         (org-with-wide-buffer
          (condition-case nil
              ;; allow only exact matches otherwise fire an error
              (let ((org-link-search-must-match-exact-headline t))
                (org-link-open-as-file target 'in-emacs))
            (error (error "Can not find org link target: " target)))
          (set-marker m (point)))))
      (_ (error ":target form must evaluate to either marker, buffer, or a string")))))

;; (org-catch--process-target '(:target "~/org/how-to-catch-a-mouse.org"))

(defun org-catch--process-body (data &optional level)
  "Get the :body element of DATA plist and adjust its headings so that min heading level would be LEVEL + 1.

If LEVEL is nil assume it 0 level."
  (when-let ((body (plist-get data :body))
             (body (substring-no-properties body))
             (level (or level 0)))
    (with-temp-buffer
      (insert body)
      (goto-char (point-min))
      (let (body-levels)
        (save-excursion
          (while (re-search-forward org-outline-regexp-bol nil 'noerror)
            (push (org-outline-level) body-levels)))
        (if-let ((body-levels (seq-reverse body-levels))
                 (body-min-level (seq-min body-levels)))
            ;; if there are headings adjust is so the the min heading level is 'level + 1'
            (progn
              (while-let (((re-search-forward org-outline-regexp-bol nil 'noerror))
                          (new-level (+ 1 level (- (pop body-levels) body-min-level)))
                          (heading-prefix (concat (make-string new-level ?*) " ")))
                (replace-match heading-prefix))
              (buffer-string))
          ;; if there are no headings return body as is
          body)))))

;; (org-catch--process-body '(:body "*** Three\n** Two (the min)\n******** Many") 3)

(defun org-catch--process-datetree (data)
  "Get date from :datetree in DATA for `org-datetree-find-date-create'. See `org-catch--file'."
  (when-let ((datetree (plist-get data :datetree)))
    (pcase datetree
      ('t (calendar-gregorian-from-absolute (org-today)))
      ((pred stringp)
       (calendar-gregorian-from-absolute
        (time-to-days
         (let (org-end-time-was-given org-time-was-given)
           (org-read-date nil 'to-time datetree))))))))

;; (org-catch--process-datetree '(:datetree "2024-04-25"))
;; (org-catch--process-datetree '(:datetree "2024-04-25 12:30"))
;; (org-catch--process-datetree '(:datetree "2024-04-25 Wed 12:30"))
;; (org-catch--process-datetree '(:datetree "2024-04-25 12:30-14:00"))
;; (org-catch--process-datetree '(:datetree t))
;; (org-catch--process-datetree '(:datetree nil))

(defvar org-catch--special-keywords '(:item
                                      :body
                                      :target
                                      :datetree
                                      :edit
                                      :backref)
  "List of special keys to which `org-catch--keyword-processors' does not apply")

(defvar org-catch--keyword-processors
  '(:tags   (lambda (key val) (org-set-tags val))
    :todo   (lambda (key val) (org-todo val))
    :effort (lambda (key val) (org-set-effort nil val))
    :logbook (lambda (key val)
               (when val
                 (save-excursion
                   (goto-char (org-log-beginning 'create))
                   (insert val))))
    ;; just for doing any funcall at target
    :funcall (lambda (key val)
               (if (listp val)
                   (apply (car val) (cdr val))
                 (funcall val)))
    ;; org-plan.el
    :plan   (lambda (key val) (when (require 'org-plan nil 'noerror)
                                (org-plan-to-drawer nil val)))
    ;; everything else used as org entry properties
    _       (lambda (key val) (org-set-property (substring (symbol-name key) 1) val)))
  "Functions for seting keywords on target org entry.")

(defun org-catch--process-keywords (data)
  "Process and apply all keywords exept `org-catch--special-keywords' at target (e.g., target org entry). DATA is a plist created from initial template."
  (save-excursion
    (org-back-to-heading 'invisible-ok)
    (seq-do
     (pcase-lambda (`(,key . ,val))
       (unless (or (not val)
                   (member key org-catch--special-keywords))
         ;; apply special keywords and properties
         (save-excursion
           (funcall (or (plist-get org-catch--keyword-processors key)
                        (plist-get org-catch--keyword-processors '_))
                    key val))))
     (map-pairs data))))

(defun org-catch--file (data)
  "File all relevant DATA at target (see `org-catch--process-keywords'). DATA is a plist created from initial template.

Returns backreference to target where thigns were files as a list '(target-file target-id target-item)."
  (let* ((target (org-catch--process-target data))
         (target-buffer
          (pcase target
            ('nil (current-buffer))
            ((pred bufferp) target)
            ((pred markerp) (marker-buffer target))))
         (target-point
          (pcase target
            ('nil (point))
            ((pred markerp) target)))
         (item (plist-get data :item))
         (item (when item (string-trim item)))
         (datetree (org-catch--process-datetree data))
         (backref (plist-get data :backref))
         ;; for storing backref
         target-id target-file target-item)
    (with-current-buffer target-buffer
      (save-mark-and-excursion
        (deactivate-mark 'force)
        ;; in case the buffer is narrowed
        (save-restriction
          (widen)
          ;; move point to target
          (when target-point (goto-char target-point))
          ;; insert datetree and move there
          (when datetree
            (org-datetree-find-date-create
             datetree
             ;; if target is heading add datetree there
             (when (and target-point
                        (org-at-heading-p))
               'subtree-at-point))
            (setq target-point (point)))
          ;; create new heading and set target-point to bol
          (when item
            (if (and target-point
                     (not (org-before-first-heading-p))
                     (not (equal (point) (point-max))))
                (progn
                  (org-insert-heading-respect-content 'invisible-ok)
                  (insert item)
                  ;; unless entry is created at point (i.e. target nil) demote it
                  (when target (org-demote)))
              ;; else append to the file as top level entry
              (goto-char (point-max))
              (insert "\n* " item))
            (save-excursion
              (org-back-to-heading 'invisible-ok)
              (setq target-point (point))))
          ;; insert body (can be with prop and other drawers)
          (when-let ((body (org-catch--process-body data (org-outline-level))))
            (save-excursion
              ;; for inserting props and stuff with body
              (when item (setq body (concat "\n" (string-trim body) "\n\n")))
              (insert body)))
          ;; write all org keywords and properties if it is entry
          (when (and target-point
                     (goto-char target-point)
                     (not (org-before-first-heading-p)))
            (org-catch--process-keywords data))
          ;; store backref
          (when backref
            (setq target-file (buffer-file-name target-buffer))
            (when (derived-mode-p 'org-mode)
              (unless (org-before-first-heading-p)
                (setq target-item (org-get-heading t t t t))
                (setq target-id (org-id-get-create)))))
          ;; save the buffer
          (save-buffer))))
    ;; return back reference info
    (list target-file target-id target-item)))

;; (org-catch--file
;;  (org-catch--record `(:item "Read a book"
;;                       :target "~/org/how-to-catch-a-mouse.org::*Todos"
;;                       :datetree read-time
;;                       :todo "TASK"
;;                       :tag (read nil "read" "book"))))


(defun org-catch--edit (data)
  "Edits the context in which the `org-catch' was called (e.g., remove active region or subtree, wrap it in bold) at the very end of filing process. It gets the :edit keyword from DATA. :edit value should be a function without args that will be called at point in the very end."
  (when-let ((edit (plist-get data :edit)))
    (and (or (functionp edit)
             (error ":edit should be a function"))
         (or (equal (func-arity edit) '(0 . 0))
             (error ":edit function should be without arguments"))
         (funcall edit))))

;; (org-catch--edit (org-catch--record
;;                   '(:item "Hello everybody!"
;;                     :edit (lambda ()
;;                             (when (region-active-p)
;;                               (delete-region (region-beginning) (region-end)))))))

(defun org-catch--backref (data target-ref-list)
  "Insert a backreference to filing target at point. TARGET-REF-LIST is a list '(target-file target-id target-item) that `org-catch--file' returns after filing. Uses :backref keyword from DATA plist. :backref value should be a function with FILE, ID and ITEM args that returns a string (e.g. an org link)."
  (when-let ((backref (plist-get data :backref))
             ((or (functionp backref)
                  (error ":backref should be a function")))
             ((or (equal (func-arity backref) '(3 . 3))
                  (error ":backref function should accept 3 args  FILE, ID and ITEM")))
             (backref (apply backref target-ref-list))
             ((or (stringp backref)
                  (error ":backref should return string to be inserted"))))
    (insert backref)))

;; (org-catch--backref (org-catch--record
;;                      '(:item "Hello from there!"
;;                        :backref (lambda (_file id _item) (concat "\n\n[[" id "][" :item "]]"))))
;;                     '("file.org" "org:123" nil))

;; [[org:123][Hello from there!]]


;;;; --------------
;;;; main interface
;;;; --------------

(defun org-catch (template &optional arg)
  "Creates processes the TEMPLATE plist that describes where and what to file and files it with possible side effects (e.g., edits at point, insertion of backreference). This function meant to be used for defining specific user's commands for various captures/catches (e.g. org-catch-task, etc.).

The TEMPLATE is a plist. Most of the plist keys in TEMPLATE would correspond to either special org properties (e.g., `:tags', `:effort') or just arbitrary org property name to set at the target org entry. There are several key symbols (e.g., `:target', `:datetree', `:backref') which provides directives, e.g., where to file things, see `org-catch--special-keywords', and some side effects, e.g., editing context at point, inserting backreference, etc. For example, a command for catching a new TODO at point with the title either from active region of from org list item and with inserting a backreference to it could look like:

  (defun org-catch-todo-at-point ()
    (interactive)
    (org-catch
     `(:tags read-tags
       :item (or region list-item read)
       :todo \"TODO\"
       :edit (or delete-region
                 delete-list-item)
       :backref (when (or region list-item)
                  (make-link :item))

One of the key feature is that the order of key value pairs in TEMPLATES defines the order in which the actuall values would be obtained, e.g., from user input. Contrary to `org-capture' functionality this allows to control the order of any questions to user during the creation of the record (e.g., first as for refile target and then for the entry's header).

The forms of TEMPLATE plist values are specified in `org-catch--preprocess-form'.

Note: If :target is just an org file name (i.e. without headings) then the mark is set to `(point-max)', i.e. at the end of file.

ARG is meant for passing a universal argument for accesisng DATA options (see `org-catch--preprocess-form')."
  (when-let ((data (or (org-catch--record template arg)
                       (error "org-catch--record does not return any data")))
             (backref (prog1 (org-catch--file data)
                        (org-catch--edit data))))
    (org-catch--backref data backref)))

;; (org-catch
;;  '(:item (or region list-item read)
;;    :body region
;;    :backref (lambda (id file) (concat "\n\n[[" id "][":item "]]"))))

;;;; -------
;;;; provide
;;;; -------

(provide 'org-catch)

;; org-catch.el ends here
