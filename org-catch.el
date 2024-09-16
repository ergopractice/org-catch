;; - Reference from [[associate-id:org:dylij2e0ofj0][Simple captures with *org-catch.el*]] on [2024-09-02 Mon 14:08]

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
;;;; helpers
;;;; -------

;;; helpers for helpers ("with" macros for getters and removers)

(defmacro org-catch---with-paragraph (&rest body)
  "Eval BODY with P1 and P2 vars bind for current paragraph boundaries."
  `(unless (or
            ;; do not work on heading-or-item
            (and (derived-mode-p 'org-mode)
                 (org-at-heading-or-item-p))
            ;; check if we are on the text line
            (and (looking-at-p "[ \t]*\n")
                 (looking-back "\n[ \t]*")))
     (org-with-wide-buffer
      (let ((p1-header
             ;; search back for heading
             (save-excursion
               (when (and (derived-mode-p 'org-mode)
                          (re-search-backward org-heading-regexp nil t))
                 (org-end-of-meta-data t)
                 (point))))
            (p1-paragraph
             ;; search back for paragraph separator "/n/n"
             (save-excursion
               (when (re-search-backward "\n[ \t]*\n" nil t)
                 (re-search-forward "\n[ \t]*\n" nil t)
                 (point))))
            (p2-header
             ;; search forward for heading
             (save-excursion
               (when (and (derived-mode-p 'org-mode)
                          (re-search-forward org-heading-regexp nil t))
                 (re-search-backward org-heading-regexp nil t)
                 (point))))
            (p2-paragraph
             ;; search forward for paragraph separator "/n/n"
             (save-excursion
               (when (re-search-forward "\n[ \t]*\n" nil t)
                 (re-search-backward "\n[ \t]*\n" nil t)
                 (point)))))
        (when-let ((p1 (or (and p1-header p1-paragraph
                                (max p1-header p1-paragraph))
                           p1-header
                           p1-paragraph
                           (point-min)))
                   (p2 (or (and p2-header p2-paragraph
                                (min p2-header p2-paragraph))
                           p2-header
                           p2-paragraph
                           (point-max)))
                   ((not (= p1 p2))))
          ,@body)))))


(defmacro org-catch---with-list (&rest body)
  "Eval BODY with P1 and P2 vars (markers) bind for current list item body (i.e., sublist) boundaries."
  `(when-let (((org-at-item-p))
              (item-begin (org-in-item-p))
              (p1 (save-excursion
                    (let ((m (make-marker)))
                      (goto-char item-begin)
                      (re-search-forward org-list-full-item-re)
                      (set-marker m (point)))))
              (p2 (save-excursion
                    (let ((m (make-marker)))
                      (org-end-of-item)
                      (backward-char)
                      (set-marker m (point)))))
              ((/= p1 p2)))
     ,@body))

(defmacro org-catch---with-list-item (&rest body)
  "Eval BODY with P1 and P2 vars (markers) bind for current list item boundaries."
  `(when-let (((org-at-item-p))
              (item-begin (org-in-item-p))
              (p1 (save-excursion
                    (let ((m (make-marker)))
                      (goto-char item-begin)
                      (re-search-forward org-list-full-item-re)
                      (set-marker m (point)))))
              (p2 (save-excursion
                    (let ((m (make-marker)))
                      (end-of-line)
                      (set-marker m (point)))))
              ((/= p1 p2)))
     ,@body))


(defmacro org-catch---with-list-body (&rest body)
  "Eval BODY with P1 and P2 vars (markers) bind for current list item body (i.e., sublist) boundaries."
  `(when-let (((org-at-item-p))
              (item-begin (org-in-item-p))
              (p1 (save-excursion
                    (let ((m (make-marker)))
                      (end-of-line)
                      (forward-char)
                      (set-marker m (point)))))
              (p2 (save-excursion
                    (let ((m (make-marker)))
                      (org-end-of-item)
                      (set-marker m (point)))))
              ((/= p1 p2)))
     ,@body))


;;; helpers for getting stuff at point

(defun org-catch--helper-region (&optional no-time)
  "Gets the current region content otherwise returns nil. With NO-TIME removes timestamp in the beggining."
  (when-let (((use-region-p) )
             (str (buffer-substring-no-properties
                   (region-beginning)
                   (region-end))))
    (if no-time
        (replace-regexp-in-string
         (concat "\\`" org-ts-regexp-inactive) "" str)
      str)))

(defun org-catch--helper-region-time ()
  "Gets the current region's timestamp if present otherwise returns nil"
  (when-let (((use-region-p) )
             (str (buffer-substring-no-properties
                   (region-beginning)
                   (region-end)))
             (time (string-match (concat "\\`" org-ts-regexp-inactive) str)))
    (match-string 1 str)))

(defun org-catch--helper-header ()
  (when (derived-mode-p 'org-mode)
    (substring-no-properties
     (org-get-heading 'no-tags 'no-todo 'no-priority 'no-comment))))

(defun org-catch--helper-at-header ()
  (when (and (derived-mode-p 'org-mode)
             (org-at-heading-p))
    (substring-no-properties
     (org-get-heading 'no-tags 'no-todo 'no-priority 'no-comment))))

(defun org-catch--helper-paragraph ()
  (org-catch---with-paragraph
   (org-with-wide-buffer
    (buffer-substring-no-properties p1 p2))))

(defvar org-catch--helper--time-prefix-re "[[:space:]]*\\(\\([0-2]?[0-9]\\)\\(:[0-5]?[0-9]\\)\\(-\\([0-2]?[0-9]\\)\\(:[0-5]?[0-9]\\)\\)?\\)[[:space:]]*")

(defun org-catch--helper-list-item (&optional no-time)
  "If in org list gets list item as string. With NO-TIME arg remove time prefix if present (e.g., 5:30 or 4:20-6:00)."
  (org-catch---with-list-item
   (if-let ((str (buffer-substring-no-properties p1 p2))
            no-time)
       (replace-regexp-in-string
        org-catch--helper--time-prefix-re "" str)
     str)))


(defun org-catch--helper-list-body ()
  "If in org list gets list item as string. With NO-TIME arg remove time prefix if present (e.g., 5:30 or 4:20-6:00)."
  (org-catch---with-list-body
   (when-let ((str (buffer-substring-no-properties p1 p2))
              ((print str))
              ((not (string-match-p "\\`[[:space:]]*\\'" str))))
     ;; fix list indentation
     (with-temp-buffer
       (insert str)
       (org-indent-region (point-min) (point-max))
       (buffer-string)))))

(defun org-catch--helper-list-time (&optional plus-days-or-date)
  "Gets the time prefix from org list element. If PLUS-DAYS-OR-DATE is number then add + that days (i.e., +2 if value is 2) and if it is a string then adds as it is as prefix."
  (org-catch---with-list-item
   (when-let ((str (buffer-substring-no-properties p1 p2))
              ((string-match org-catch--helper--time-prefix-re str)))
     (concat
      (pcase plus-days-or-date
        ((pred stringp)
         (concat plus-days-or-date " "))
        ((pred numberp)
         (concat "+" (number-to-string plus-days-or-date) " ")))
      (match-string 1 str)))))

(defun org-catch--helper-tags ()
  (when (derived-mode-p 'org-mode)
    (org-get-tags nil 'local)))

(defun org-catch--helper-at-header-tags ()
  (when (and (derived-mode-p 'org-mode)
             (org-at-heading-p))
    (org-get-tags nil 'local)))

(defun org-catch--helper-at-header-body ()
  (when (and (derived-mode-p 'org-mode)
             (org-at-heading-p))
    (org-with-wide-buffer
     (let ((p1 (save-excursion
                 ;; (org-end-of-meta-data t)
                 ;; just grab everything right after the heading
                 (org-back-to-heading)
                 (end-of-line)
                 (point)))
           (p2 (org-with-wide-buffer
                (org-end-of-subtree 'invisible-ok)
                (point))))
       (buffer-substring-no-properties p1 p2)))))


;;; functions that return deletion functions (for :edit keyword)

(defun org-catch--helper-delete-region ()
  "If region is active retun function that deletes it."
  (when (use-region-p)
    (lambda ()
      (org-with-wide-buffer
       (when-let (((use-region-p))
                  (p1 (region-beginning))
                  (p2 (region-end)))
         (delete-region p1 p2)
         (goto-char p1))))))

(defun org-catch--helper-delete-paragraph ()
  "If at paragraph (i.e. text surrounded by two new lines or by org headers) then return function that deletes this paragraph"
  (unless (or
           ;; do not work on heading-or-item
           (and (derived-mode-p 'org-mode)
                (org-at-heading-or-item-p))
           ;; check if we are on the text line
           (and (looking-at-p "[ \t]*\n")
                (looking-back "\n[ \t]*")))
    `(lambda ()
       (org-with-wide-buffer
        (org-catch---with-paragraph
         (delete-region p1 p2)
         (goto-char p1))))))

;; (funcall (org-catch--helper-delete-paragraph))

(defun org-catch--helper-delete-list-item ()
  (when (and (org-at-item-p) (org-in-item-p))
    '(lambda ()
       (org-catch---with-list-item
        (org-with-wide-buffer
         (delete-region p1 p2)
         (goto-char p1))))))


(defun org-catch--helper-delete-list ()
  (when (and (org-at-item-p) (org-in-item-p))
    '(lambda ()
       (org-catch---with-list
        (org-with-wide-buffer
         (delete-region p1 p2)
         (goto-char p1))))))

(defun org-catch--helper-delete-at-header-subtree ()
  '(lambda ()
     (when-let (((org-at-heading-p 'invisible-not-ok))
                (p1 (save-excursion
                      (org-back-to-heading)
                      (point)))
                (p2 (org-with-wide-buffer
                     (org-end-of-subtree 'invisible-ok)
                     (point))))
       (org-with-wide-buffer
        (delete-region p1 p2)
        (goto-char p1)))))


;;; helpers for reading from the user input

(defun org-catch---make-mc-table (choices)
  "Create a list of multiples choice acronim keys from list of strings CHOICES ensuring there are no duplicates. Keys are downcased. Space character is reserved for `nil'"
  (if (seq-every-p 'stringp choices)
      (let ((alnum "abcdefghijklmnopqrstuvwxyz0123456789")
            keys)
        (seq-do
         (lambda (choice)
           (let ((key (thread-first
                        (replace-regexp-in-string  "[ \t\n]+" "" choice) ; reserve space for nil
                        (downcase)
                        (concat alnum)
                        (seq-uniq)
                        (seq-difference keys)
                        (car))))
             (unless key (error "No characters left for the multiple choice assignment!"))
             (push key keys)))
         choices)
        (seq-mapn 'list (reverse keys) (append choices '("'nil"))))
    (car choices)))

;; (read-multiple-choice "Hi: " (org-catch---make-mc-table '("Hello" " Hi")))


;;; readers helpers

(defun org-catch--helper-read-mc (&rest choices)
  "Read multiple choice input from user. Space character refers to nil value.

CHOICES are ether string arguments or just a list that can be passed directly to `read-multiple-choice' function (for more control)."
  (let* ((prompt (or org-catch---key-prompt "org-catch-read-mc: "))
         (choices (org-catch---make-mc-table choices))
         (choices (append choices '((?\s "'nil" "Quit and return 'nil"))))
         (choice (read-multiple-choice prompt choices)))
    (if (and (equal (car choice) ?\s)
             (equal (cadr choice) "'nil"))
        nil
      (cadr choice))))

;; (org-catch--helper-read-mc "Hi" "Hello")
;; (org-catch--helper-read-mc '((?w "Hi") (?r "Hello") (?\s "yo")))

(defun org-catch--helper-y-or-n (&optional yes no)
  "Read yes or no input from user"
  (let ((prompt (or org-catch---key-prompt "org-catch--helper-y-or-n: ")))
    (if (y-or-n-p prompt) (or yes t) no)))

;; (org-catch--helper-y-or-n "yes" "no")

(defun org-catch--helper-read-multi (&optional input separator &rest choices)
  "Read multiple strings input from user.

INPUT is a default input value

SEPARATOR can be either a string (single character that sparates values) or a list in form of `(,SEPARATOR-CHAR ,SEPARATOR-KEY ,EXIT-KEY) where SEPARATOR-CHAR is the same as separator string, SEPARATOR-KEY is a key binding for inserting that separator (SEPARATOR-CHAR) and EXIT-KEY (can be omitted) is a key binding for exiting with value nil.

CHOICES are string arguments for choices."
  (let* ((prompt (or org-catch---key-prompt
                     "org-catch--helper-read-multi"))
         (separator-key (when (listp separator) (cadr separator)))
         (exit-key (when (listp separator) (caddr separator)))
         (separator (or (when (listp separator) (car separator)) separator ",")))
    (let ((crm-local-completion-map
           (let ((map (make-sparse-keymap)))
             (require 'crm)
             (set-keymap-parent map crm-local-completion-map)
             (and exit-key (stringp exit-key)
                  (define-key map (kbd exit-key) 'exit-minibuffer))
             (and separator (stringp separator)
                  separator-key (stringp separator-key)
                  (define-key map (kbd separator-key) separator))
             map))
          (vertico-map (when (require 'vertico nil 'noerror)
                         (let ((map (make-sparse-keymap)))
                           (set-keymap-parent map vertico-map)
                           (and exit-key (stringp exit-key)
                                (define-key map (kbd exit-key) 'vertico-exit-input))
                           (and separator (stringp separator)
                                separator-key (stringp separator-key)
                                (define-key map (kbd separator-key) separator))
                           map)))
          (crm-separator (concat "[ \t]*" separator "[ \t]*")))
      (completing-read-multiple prompt choices nil nil input))))

;; (org-catch--helper-read-multi nil '(":" ";" "SPC") "a" "b" "c")

(defun org-catch--helper-read (&optional input &rest choices)
  "Read imput from user."
  (let ((prompt (or org-catch---key-prompt "org-catch--helper-read")))
    (if choices
        (completing-read prompt choices nil nil input)
      (read-string prompt input))))

(defun org-catch--helper-read-match (&optional input &rest choices)
  "Read imput from user."
  (let ((prompt (or org-catch---key-prompt "org-catch--helper-read")))
    (if choices
        (completing-read prompt choices nil 'require-match input)
      (error "Some choices should be set after the `read-match' symbol to match against"))))


;;; helpers for reading org tags

(defvar org-catch---tags-definition-files nil
  "List of files with org tags definitions. Each tag's definition is an org entry which title has the name of the tag wrapped in '='. E.g., '* definition of =@home= tag'")

(defun org-catch---get-defined-tags ()
  "Collect all org tags definitions (from headings with tag :tag: that contain '=tag=' substring) from `org-catch-tags-definition-files'"
  (when-let ((tag-files (seq-filter 'file-exists-p org-catch---tags-definition-files))
             (org-use-tag-inheritance t))
    (seq-filter
     'identity
     (mapcar
      (lambda (x)
        (when (string-match "=\\([^=]+\\)=" x)
          (match-string 1 x)))
      (org-map-entries
       (lambda ()
         (substring-no-properties
          (org-get-heading 'no-tags 'no-todo 'no-priority 'no-comment)))
       "tag"
       tag-files)))))

(defun org-catch--helper-read-tags (&optional init-tags)
  "Read tags from user with autocompletion from `org-catch-tags-definition-files' and return it as a list."
  (let ((crm-local-completion-map
         (let ((map (make-sparse-keymap)))
           (require 'crm)
           (set-keymap-parent map crm-local-completion-map)
           (define-key map (kbd ";") ":")
           map))
        (vertico-map (when (featurep 'vertico)
                       (let ((map (make-sparse-keymap)))
                         (require 'vertico)
                         (set-keymap-parent map vertico-map)
                         (define-key map (kbd "SPC") 'vertico-exit-input)
                         (define-key map (kbd ";") ":")
                         map)))
        (crm-separator "[ \t]*:[ \t]*"))
    (completing-read-multiple
     "Type tags seprarated by column: "
     (org-catch---get-defined-tags) nil nil init-tags)))


;;; helpers for reading time 

(defvar org-catch--helper-read-time--last-end-time nil)

(defun org-catch--helper-read-time (&optional default-time ignore-last-end-time)
  (let ((prompt (or org-catch---key-prompt
                    "org-catch--helper-read-time"))
        org-time-was-given
        org-end-time-was-given)
    (concat
     (org-read-date 'with-time nil nil prompt nil
                    (or (and (stringp default-time) default-time)
                        (unless ignore-last-end-time
                          org-catch--helper-read-time--last-end-time)))
     (when (and org-time-was-given org-end-time-was-given)
       (setq org-catch--helper-read-time--last-end-time org-end-time-was-given)
       (concat "-" org-end-time-was-given)))))

;; (org-catch--helper-read-time)

;;; helpers for getting time and date

(defun org-catch--helper-now ())

(defun org-catch--helper-yesterday ())

(defun org-catch--helper-tomorrow ())


;;; helpers for reading target (ol - outline)

(defun org-catch--helper-read-ol (&optional files filter input)
  "Reads org outline path and returns a marker. The FILES FILTER and INPUT arguments accept following:

FILES argument can be either or a list of the folloing:
    nil                   - defaults to `org-refile-targets'
    buffer                - current buffer
    agenda                - current agenda files
    agenda-file           - choose file(s) from current agenda
    (FILE1 FILE2...)      - list of strings with files paths
    ...                   - everything else assumed to be `org-refile-targets' direct specs             
- values that requires `org-agenda-sets' package:
    read-agenda-set            - choose the agenda files set for targets
    read-agenda-set-file       - choose the agenda files set and a file for targets
    (agenda-set SET)      - use agenda files SET for targets
    (agenda-set-file SET) - use agenda files SET and choose a file for targets

FILTERS argument (basically any predicate function but here are some helpers sortcuts `org-catch--helper-'):
    subtree-p             - tests if point is within current subtree
    (subtodo-p TODOS)     - tests if point is withing colosest parent subtree with TODOS keyword
    (subtag-p TAGS)       - tests if point is withing colosest parent subtree with TAGS
    (todo-p &op TODOS)    - tests if point is in todo entry (optionally specify states with TODOS)
    (no-todo-p &op TODOS) - tests if point is not in todo entry (optionally with TODOS)
    any-todo-p            - tests if point is in entry with any todo
    done-p                - tests if point is in entry with done state
    (tag-p &op INHERIT TAGS)  - tests if point is in entry with tags. If TAGS is provided check for those and if INHERIT is not nil consider inherited tags as well.
    (no-tag-p &op INHERIT TAGS)
    (every-pred-p)        - function for combining predicate functions as with `and'
    (some-pred-p)         - function for combining predicate functions as with `or'
    
INPUT argument:
  STRING                  - default input string
  region                  - use current region as default input"
  (let* ((prompt (or org-catch---key-prompt
                     "org-catch--helper-read-ol: "))
         (org-refile-targets
          (if files
              (if (or (when (or (symbolp files)
                                (list-of-strings-p files))
                        ;; convert to list for mapcar
                        (setq files (list files)))
                      (when (stringp files)
                        ;; confert to double list
                        (setq files (list (list files))))
                      (proper-list-p files))
                  (mapcar (lambda (f)
                            (pcase f
                              ;; current buffer
                              ('buffer (if (derived-mode-p 'org-mode)
                                           '(nil . (:maxlevel . 10))
                                         (message "org-catch--helper-read-ol: current buffer is not in org-mode, defaulting to `org-agenda-files'")
                                         `('org-agenda-files . (:maxlevel . 10))))
                              ;; agenda
                              ('agenda `(,org-agenda-files . (:maxlevel . 10)))
                              ('agenda-file `(,(completing-read-multiple (concat prompt " (agenda file): ") org-agenda-files) . (:maxlevel . 10)))
                              ;; list of strings assumed to be a list of files
                              ((pred list-of-strings-p) `(,f . (:maxlevel . 10)))
                              ;; everything else is assumed to be a cons element of`org-refile-targets' direct specs
                              (`(,_files . (,_key . ,_val)) f)
                              (_ (error "Bad FILES arg specification"))))
                          files)
                (error "Bad FILES arg specification"))
            ;; if files nil use current defauls
            org-refile-targets))
         ;; elements filtering
         (org-refile-target-verify-function filter)
         ;; advice fucntion for injecting devault input in refile dialog
         ;; args of `completing-read'
         ;; prompt collection &optional predicate require-match initial-input hist def inherit-input-method
         ;; 0      1                    2         3             4
         ;;                                                     ^ it is 4th argument
         (completing-read-inject-default-input
          `(lambda (args) (setf (nth 4 args) ,input) args)))
    (save-mark-and-excursion
      (deactivate-mark 'force)
      (when-let ((target
                  (unwind-protect
                      (progn (advice-add 'completing-read :filter-args completing-read-inject-default-input)
                             ;; clear cache because it fetches stuff from it and  filters does not have effect
                             (when org-refile-target-verify-function (org-refile-cache-clear))
                             (org-refile-get-location prompt nil t)) 
                    (advice-remove 'completing-read completing-read-inject-default-input)))
                 (target-file (nth 1 target))
	         (target-pos (or (nth 3 target)
                                 ;; for the top level entries (i.e., just a file) use the end of file
                                 (with-current-buffer (find-file-noselect file)
                                   (org-with-wide-buffer (point-max)))))
                 (m (make-marker)))
        (set-marker m target-pos (find-file-noselect target-file))
        ;; clear cache if we use filters because
        (when filter (org-refile-cache-clear))
        ;; return marker
        m))))

;; (org-catch--helper-read-ol 'buffer (org-catch--helper-subtree-p))

;; (org-catch--helper-read-ol (org-catch--helper-read-agenda-set) (org-catch--helper-todo-p))

;; (org-catch--helper-read-ol (org-catch--helper-agenda-set "orgzly") (org-catch--helper-todo-p) (org-catch--helper-region))

;; (org-catch--helper-read-ol (org-catch--helper-agenda-set "orgzly") (org-catch--helper-todo-p) (org-catch--helper-region))




;;; helpers for olpath reader helper (scope filtering)

(defun org-catch--helper-subtree-p ()
  "Returns a function that checks if point is in current subtree."
  (org-with-wide-buffer
   `(lambda ()
      ;; check if we are in the same buffer
      (unless (equal (current-buffer) ,(current-buffer))
        (error "`org-catch--helper-subtree-p' filter could be only used in the current buffer"))
      (or ,(org-before-first-heading-p)
          (and (>= (point)
                   ,(progn (org-back-to-heading 'invisible-ok) (point)))
               (<= (point)
                   ,(progn (org-end-of-subtree 'invisible-ok) (point)))
               t)))))

;; (functionp (org-catch--helper-subtree-p))

(defun org-catch--helper-subtodo-p (todos)
  "Returns a function that checks if point is in the parent subtree with one of TODOS todo state that is closest to current point. See `org-network-climb-subtree' for details."
  (if (require 'org-network nil 'noerror)
      (org-with-wide-buffer
       `(lambda ()
          ;; check if we are in the same buffer
          (unless (equal (current-buffer) ,(current-buffer))
            (error "`org-catch--helper-subtodo-p' filter could be only used in the current buffer"))
          (or ,(org-before-first-heading-p)
              (and (>= (point)
                       ,(progn (eval `(org-network-climb-subtree :todo ,@todos)) (point)))
                   (<= (point)
                       ,(progn (org-end-of-subtree 'invisible-ok) (point)))
                   t))))))

(defun org-catch--helper-subtag-p (tags)
  "Returns a function that checks if point is in the parent subtree that has one of TAGS set that is closest to current point. See `org-network-climb-subtree' for details."
  (if (require 'org-network nil 'noerror)
      (org-with-wide-buffer
       `(lambda ()
          ;; check if we are in the same buffer
          (unless (equal (current-buffer) ,(current-buffer))
            (error "`org-catch--helper-subtag-p' filter could be only used in the current buffer"))
          (or ,(org-before-first-heading-p)
              (and (>= (point)
                       ,(progn (eval `(org-network-climb-subtree :tag ,@tags)) (point)))
                   (<= (point)
                       ,(progn (org-end-of-subtree 'invisible-ok) (point)))
                   t))))))

(defun org-catch--helper-todo-p (&rest todos)
  "Returns function that checks if the point is in entry with TODOS or with any 'todo state (see `org-entry-is-todo-p')"
  (if todos
      `(lambda () (and (member (org-get-todo-state) ',todos) t))
    (lambda () (and (org-entry-is-todo-p) t))))

(defun org-catch--helper-done-p ()
  "Returns a function that checks if the point is in a done entry."
  (lambda () (and (member (org-get-todo-state) org-done-keywords) t)))

(defun org-catch--helper-any-todo-p ()
  "Returns a function that checks if the point is in an entry with any todo state."
  (lambda () (and (org-get-todo-state) t)))

(defun org-catch--helper-no-todo-p (&rest todos)
  "Returns function that checks if the point is not in entry with TODOS or with none of 'todo state (see `org-entry-is-todo-p'"
  (if todos
      `(lambda () (not (member (org-get-todo-state) ',todos)))
    (lambda () (not (org-get-todo-state)))))


(defun org-catch--helper-tag-p (&optional tags inherit)
  "Returns a function that checks if the point is in an entry with TAGS. TAGS can be either string or list of strings. INHERIT is a boolean that specifies if tags should be inherited (see `org-use-tag-inheritance'). If TAGS is nil check if there any local tags at all (the INHERIT is ignored)."
  (if tags
      `(lambda ()
         (let ((org-use-tag-inheritance ,inherit)
               (tags ',(cond ((stringp tags)
                              (list tags))
                             ((list-of-strings-p tags)
                              tags)
                             (t (error "TAGS is misspcified")))))
           (and (seq-intersection (org-get-tags nil 'local) tags) t)))
    (lambda () (and (org-get-tags nil 'local) t))))

(defun org-catch--helper-no-tag-p (&optional tags inherit)
  "Returns a function that checks if the point is in an entry without TAGS. INHERIT is a boolean that specifies if tags should be inherited. If TAGS is nil check if there are no local tags at all (the INHERIT is ignored)."
  (if tags
      `(lambda ()
         (let ((org-use-tag-inheritance ,inherit)
               (tags ',(cond ((stringp tags)
                              (list tags))
                             ((list-of-strings-p tags)
                              tags)
                             (t (error "TAGS is misspcified")))))
           (not (seq-intersection (org-get-tags nil 'local) tags))))
    (lambda () (not (org-get-tags nil 'local)))))

(defun org-catch--helper-every-pred-p (&rest preds)
  "Returns a function that checks if all the PREDS functions return non nil."
  `(lambda () (and (seq-every-p 'identity (mapcar 'funcall ',preds)) t)))

;; (org-catch--helper-every-pred-p
;;  (org-catch--helper-tag-p "el")
;;  (org-catch--helper-todo-p "TASK"))

(defun org-catch--helper-some-pred-p (&rest preds)
  "Returns a function that checks if some of the PREDS functions return non nil."
  `(lambda () (and (seq-some 'identity (mapcar 'funcall ',preds)) t)))


;;; helpers for reading agenda-sets (separate package)

(defun org-catch--helper-read-agenda-set (&optional input)
  "Rerurns list of files from agenda sets chosen by user. Requires `org-agenda-sets' package."
  (if (require 'org-agenda-sets nil 'noerror)
      (org-agenda-sets-get  
       (completing-read-multiple
        (concat org-catch---key-prompt "[agenda set]: ")
        (mapcar 'car (org-agenda-sets)) nil 'require-match input nil org-agenda-sets-default-set))
    (error "`org-catch--helper-read-agenda-set' requires `org-agenda-sets' package to be installed")))


(defun org-catch--helper-agenda-set (sets)
  "Rerurns list of files from agenda SETS. Requires `org-agenda-sets' package."
  (if (require 'org-agenda-sets nil 'noerror)
      (org-agenda-sets-get sets)
    (error "`org-catch--helper-read-agenda-set' requires `org-agenda-sets' package to be installed")))

(defun org-catch--helper-agenda-set-read-file (sets &optional input)
  "Reads file(s) from user out of agenda SETS. Requires `org-agenda-sets' package."
  (if (require 'org-agenda-sets nil 'noerror)
      (completing-read-multiple (concat org-catch---key-prompt "[file]: ") (org-agenda-sets-get sets) nil 'requre-match input)
    (error "`org-catch--helper-read-agenda-set' requires `org-agenda-sets' package to be installed")))

(defun org-catch--helper-read-agenda-set-file (&optional input-set input-file)
  "Reads file(s) from user out of agenda sets chosen by user. Requires `org-agenda-sets' package."
  (if (require 'org-agenda-sets nil 'noerror)
      (completing-read-multiple
       (concat org-catch---key-prompt "[file]: ")
       (org-agenda-sets-get  
        (completing-read-multiple
         (concat org-catch---key-prompt "[agenda set]: ")
         (mapcar 'car (org-agenda-sets)) nil 'require-match input-set nil org-agenda-sets-default-set))
       nil 'requre-match input-file)
    (error "`org-catch--helper-read-agenda-set' requires `org-agenda-sets' package to be installed")))

;;;; helper for in steps olpath helper
;;;; (sequential prompting for practice or projectcs)

(defun org-catch--helper-read-ol-insteps (filter1 &optional filter2 files input)
  "A wrapper fro `org-catch--helper-read-ol' that narrows targets in two steps. E.g., read the subtree based on tag or todo filtering and then read from its children.
FILES and FILTER1 are for the first olpath reading (same as in `org-catch--helper-read-ol')
FILTER2 and INPUT are for the second olpath reading (its `org-catch--helper-read-ol' FILES arg is 'buffer). FILTER2 function should be buffer independent (e.g., '(org-catch--helper-todo-p \"TASK\"))' and not '(org-catch--helper-subtree-p)' as the returned function will be created for the current buffer's subtree)."
  (let ((m (let (org-refile-use-outline-path)
             (org-catch--helper-read-ol files filter1)))
        (filter2 (or filter2 (lambda () t))))
    (with-current-buffer (marker-buffer m)
      (org-with-wide-buffer
       (goto-char m)
       (org-catch--helper-read-ol
        'buffer
        (org-catch--helper-every-pred-p
         (org-catch--helper-subtree-p)
         filter2)
        input)))))

;; (org-catch--helper-read-ol-insteps (org-catch--helper-tag-p nil "practice"))

;; (org-catch--helper-read-ol-insteps (org-catch--helper-tag-p nil "practice")
;;                                        (org-catch--helper-todo-p "TASK"))



;;; helpers for back references

(defun org-catch--helper-make-link (&optional name time wrap)
  "Return a function that makes an org link to target's id or target's file if id is not available.The optional arg NAME is the link's text. WRAP is a string that wraps the link (e.g., for WRAP '+' it will return '+[[id][link]]+').

The returned function accepts 3 arguments FILE, ID and ITEM - and is called by `org-catch--backref' at the very end."
  `(lambda (file id item)
     (concat
      ,(when (and (stringp time)
                  (string-match org-catch--helper--time-prefix-re time)) (concat
                  (match-string 1 time) " "))
      ,(when (stringp wrap) wrap)
      "[["
      (or (and (stringp id) (concat "id:" id))
          (and (stringp file) (concat "file:" file)))
      "]"
      (when-let ((name (or ,name item)))
        (concat "[" name "]"))
      "]"
      ,(when (stringp wrap) wrap))))

;; (funcall (org-catch--helper-make-link "hello")  "1223" nil)

;; (funcall (org-catch--helper-make-link "hello" "2024-09-06 10:00-11:00")  "1223" nil)

;;;; -------
;;;; provide
;;;; -------

(provide 'org-catch)

;; org-catch.el ends here
