;; -------->>  [[file:org-catch.src.org::*require][require:1]]
(require 'org)
(require 'org-clock)
(require 'subr-x)                       ; when-let, thread-first
(require 'map)                          ; map-keys
;; --------<<  require:1 ends here



;; -------->>  [[file:org-catch.src.org::*engine][engine:1]]
;;;; ---------------
;;;; form processing
;;;; ---------------

(defvar org-catch--helpers-prefix "org-catch--helper-"
  "Prefix for helper functions that intended to be used in `org-catch' DATA argument. This prefix can be omitted in the DATA forms (values for keys) for readability. Mind, however, possible namespace clashes.")


(defvar org-catch--kwd-substitution-prefix "_"
  "Prefix for symbols in the template that should be substituted with collected records, e.g, if template is (:greeding \"hello\" :item (concat _greeding \"world\")) and default prefix is _ then :item would be substituted by (concat \"hello\" \"world\") before the evaluation.")

(defun org-catch--preprocess-form (form &optional pos collected-pairs)
  "Recursively preprocess the FORM or if FORM is a list its element at the POS position when called recursively. COLLECTED-PAIRS is plist of previously collected values for keys that can be used for substitution if FORM has ':symbol' pattern.

The following substitution rules are applied:

- if the FORM is a list of alists (no longer than 4 elements) then select element according to universal prefix. The car of each alist is universal prefix value and cdr is body to preprocess and evaluate. See `org-catch--data'.
- if FORM is a list then each element is preprocessed recursively
- if FORM is a symbol and matches a prefixed helper function (see `org-catch--helpers-prefix') it is substituted with this function, i.e., `region' -> `org-catch--region'. Also if the FORM matches a prefixed helper function and is not a first symbol in a list (controlled with POS arg) it will be wrapped in a funcall, i.e. region -> (org-catch--region)
- if FORM is a symbol that starts with '_' (`org-catch--kwd-substitution-prefix') and matches to the corresponding key's value if it was already evaluated it will be substituted, e.g., :item -> 'New item' (assuming :item key's value was already set to 'New item' earlier)
- in any other case the FORM is left as it is
- at the end the preprocessed FORM is evaluated and should return the value for template's key (see `org-catch--data')

Returns preprocessed form."
  (pcase form
    ((pred symbolp) (cond
                     ;; substitute with previously collected values
                     ((when-let ((form-name (symbol-name form))
                                 ((string-prefix-p "_" form-name))
                                 (form-kwd
                                  (intern (concat ":" (substring form-name 1))))
                                 ((plist-member collected-pairs form-kwd)))
                        (plist-get collected-pairs form-kwd)))
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
    ((pred listp)
     (let* ((form-length (safe-length form))
            (form-proper (take form-length form))
            (form-list
             (seq-map-indexed
              (lambda (f p)
                (org-catch--preprocess-form f p collected-pairs))
              form-proper))
            (form-last (last form)))
       (if-let (((not (proper-list-p
                       form-last)))
                (form-cdr
                 (org-catch--preprocess-form
                  (cdr form-last) (1+ form-length) collected-pairs)))
           `(,@form-list . ,form-cdr)
         form-list)))
    ;; (`(,form-car . ,form-cdr)
    ;;  (cons (org-catch--preprocess-form form-car 0 collected-pairs)
    ;;        (org-catch--preprocess-form form-cdr 1 collected-pairs)))
    (_ form)))

;; (org-catch--preprocess-form '(:item region) nil '(:item "Hello"))
;; (org-catch--preprocess-form :item nil '(:item "Hello"))
;; (org-catch--preprocess-form 'region)
;; (org-catch--preprocess-form '(link _item) nil '(:item "Hello"))
;; (org-catch--preprocess-form '(link . _item) nil '(:item "Hello"))
;; (org-catch--preprocess-form '(link something . _item) nil '(:item "Hello"))




(defun org-catch--template-ok-p (template)
  "Check if TEMPLATE is valid"
  (when-let (((or (plistp template)
                  (error "TEMPLATE is misformed. Shuld be plist.")))
             (keys (map-keys template))
             ((or (seq-every-p 'keywordp keys)
                  (error "TEMPLATE is misformed. The keys should be keywords.")))
             ;; only allow lists with unique keys
             (keys-uniq (seq-uniq keys))
             ((or (equal keys (seq-uniq keys))
                  (error "TEMPLATE is misformed. The keys are not unique."))))
    t))

;; (org-catch--template-ok-p '(1))
;; (org-catch--template-ok-p '(:a 1 :b 2))
;; (org-catch--template-ok-p '(:a 1 :a 2))
;; (org-catch--template-ok-p '(:a 1 b 2))

(defvar org-catch---key-prompt nil
  "For storing user prompt generated from template's key.")

(defun org-catch--data (template &optional arg)
  "Collects data for TEMPLATE. The TEMPLATE is plist with keys as symbols like `:key' and value forms that are preprocessed with `org-catch--preprocess-form' (see docs for form specification) and evaluated at point where `org-catch' is called."
  (when-let (((org-catch--template-ok-p template))
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

;; (org-catch--data '(:key ((1 . "a")
;;                          (4 . "b")
;;                          (16 . "c"))) '(4))

;; (org-catch--data '(:key ((64 . "x")
;;                          (16 . "c"))) 64)

;; (org-catch--data '(:key ((64 . "x")
;;                          (16 . "c"))) nil)

;; expect error
;; (org-catch--data '(:key ((65 . "x"))))



;;;; ------------------------------
;;;; keywords processing and filing
;;;; ------------------------------

;;; processing special keys

(defun org-catch--preprocess-target (target)
  "Process :target form. Check type and if string (org link) translate it to buffer or marker."
  (let ((m (make-marker)))
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

;; (org-catch--preprocess-target "~/org/how-to-catch-a-mouse.org")

(defun org-catch--preprocess-body (body &optional level)
  "adjust BODY's headings so that min heading level would be LEVEL + 1. If LEVEL is current outline level."
  (when-let ((body (substring-no-properties body))
             (level (or level (org-outline-level) 0)))
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

;; (org-catch--preprocess-body "*** Three\n** Two (the min)\n******** Many" 3)

(defun org-catch--preprocess-datetree (date)
  "Prepare DATE for `org-datetree-find-date-create'. See `org-catch--file'."
  (when date
    (pcase date
      ('t (calendar-gregorian-from-absolute (org-today)))
      ((pred stringp)
       (calendar-gregorian-from-absolute
        (time-to-days
         (let (org-end-time-was-given org-time-was-given)
           (org-read-date nil 'to-time date))))))))

;; (org-catch--preprocess-datetree "2024-04-25")
;; (org-catch--preprocess-datetree "2024-04-25 12:30")
;; (org-catch--preprocess-datetree "2024-04-25 Wed 12:30")
;; (org-catch--preprocess-datetree "2024-04-25 12:30-14:00")
;; (org-catch--preprocess-datetree t)
;; (org-catch--preprocess-datetree nil)


;; -----------------------------------------------------------------------------
;; Keywords

(defvar org-catch-keywords
  '(
    ;; target
    :target (:method target :doc "Setting the target where to file what was caught."
             :types (nil stringp bufferp markerp)
             :eval (org-catch--preprocess-target _val))
    :datetree (:method target-datetree
               :doc "A datetree"
               :types (nil t stringp)
               :eval (org-catch--preprocess-datetree _val))
    ;; basic inserts
    :item (:method target-item :doc "An item, i.e., a new org entry to create at target."
           :types (nil stringp)
           :eval (when _val (string-trim _val)))
    :body (:method body :doc "An item, i.e., a new org entry to create at target."
           :types (nil stringp)
           :eval (org-catch--preprocess-body _val))
    ;; evaluations with bindings from previous phases
    :init (:method eval-init
           :doc "Evaluate before embarding to target. Bind variables things."
           :eval _val)
    :before (:method eval-before
             :doc "Evaluate after inserting item to target."
             :eval-bind (eval-init))
    :after (:method eval-after
            :doc "Evaluate after inserting item to target."
            :eval-bind (eval-init eval-before))
    :final (:method eval-final
            :doc "Evaluate after inserting item to target."
            :eval-bind (eval-init eval-before eval-after))
    ;; special inserts
    :logbook (:method set-properties
              :doc "Evaluate before embarding to target. Bind variables things."
              :types (nil stringp)
              ;; alternatives :ins :inc :let :inj :eval :form :fun
              :eval (when _val
                      (save-excursion
                        (goto-char (org-log-beginning 'create))
                        (insert _val "\n"))))
    ;; org-plan.el
    :plan   (:method set-properties
             :doc "Evaluate before embarding to target. Bind variables things."
             :types (nil stringp)
             :eval (when (require 'org-plan nil 'noerror)
                     (org-plan-to-drawer nil _val)))
    ;; special org properties
    :tags   (:method set-properties
             :doc "Evaluate before embarding to target. Bind variables things."
             :types (nil stringp list-of-strings-p)
             :eval (org-set-tags _val))
    :todo   (:method set-properties
             :doc "Evaluate before embarding to target. Bind variables things."
             :types (nil stringp)
             :eval (org-todo _val))
    :effort (:method set-properties
             :doc "Evaluate before embarding to target. Bind variables things."
             :types (nil stringp)
             :eval (org-set-effort nil _val))
    ;; complex keywords (more than one method)
    ;; insert reference (org link) to target
    :insert-ref (:method eval-after
                 :doc "After the catch was filled to the target it inserts nicely formatted org link pointing to the target. The keyword expects plist with some options like ':name' for links text ':time' for inserting a time of the day in front of the link (time is matched from any iso timestamp) and ':wrap' for wrapping the link in some org markup (e.g., '+', '_'). Note that this is two method keyword so it can trigger things at different stages and pass data from one to another."
                 :types (t plistp)
                 :eval (when _val
                         (let ((target-file (buffer-file-name))
                               (target-item
                                (when (derived-mode-p 'org-mode)
                                  (unless (org-before-first-heading-p)
                                    (org-get-heading t t t t))))
                               (target-id
                                (when (derived-mode-p 'org-mode)
                                  (unless (org-before-first-heading-p)
                                    (org-id-get-create)))))
                           `((_id   . ,target-id)
                             (_file . ,target-file)
                             (_item . ,target-item)))))
    :insert-ref (:method eval-final
                 :types (t plistp)
                 :eval-bind (eval-after)
                 :eval (when _val
                         (message "Inserting stuff")
                         (insert
                          (concat
                           ;; time
                           (when-let ((time (plist-get _val :time))
                                      ((stringp time))
                                      ((string-match org-catch--helper--time-prefix-re time)))
                             (concat
                              (match-string 1 time) " "))
                           ;; org link (wrapped)
                           (plist-get _val :wrap) "[["
                           (or (and (stringp _id) (concat "id:" _id))
                               (and (stringp _file) (concat "file:" _file)))
                           "]"
                           (when-let (name (or (plist-get _val :text) _item))
                             (concat "[" name "]"))
                           "]" (plist-get _val :wrap)))))
    ;; everything else used as org entry properties (glob pattern)
    :*      (:method set-properties
             :doc "Evaluate before embarding to target. Bind variables things."
             :types (nil stringp)
             :eval (when _val (org-set-property (substring (symbol-name _key) 1) _val))))
  "Keywords specification as plist (:KEYWORD SPECS ...). Each keyword's specification SPECS is also a plist with the following keys and values:
':method'    - a symbol for the meaning/category of the given keyword. Allowed values:
         'target' - either string, buffer or marker that specifies target where to file the catch
         'target-datetree' - date for navigating/creating datetree heading at the target
         'target-item' - new org heading to insert at target
         'target-body' - body text to insert at target
         'eval-init' - form to evaluate before moving point to target
         'eval-before' - form to evaluate at target before inserting new entry 'target-item'
         'eval-after' - form to evaluate at target after inserting new entry
         'eval-final' - form to evaluate after filing target with point back at initial context
':types'  - Specification for allowed values/types check. A list of either function symbols (assumed to be predicates for type check) or other values (assumed to be allowed values).
':doc'  - Documentation for keyword.
':eval' - Form to evaluate during keyword processing. The following variables are lexically bind during evaluation
        '_val' - catched value for the keyword
        '_key' - keyword symbol (as 'keywordp')
        '_kwd' - keyword string (i.e., symbol name without semicolon)
':eval-bind' - A list of ':method' symbols which evaluated results should be used as lexical bindings for evaluation of keyword value or ':eval' form when it is provided. Basically it allows to pass some data, e.g., for inserting back references, between different evaluation phases (evaluations at initial context, evaluations at target and final evaluations at context).")

(defun org-catch---glob-re (str)
  "Creates a regular expresion from STR string that can also match glob paterns"
  (let ((str (regexp-quote str)))
    (concat "\\`"
            (string-replace "\\*" ".*" str)
            "\\'")))

(defun org-catch---check-type (key val types)
  "Given TYPES check VAL and throw an informative error if VAL does not fit TYPES.
TYPES is a list of TYPEs. If TYPE is a function assume it is predicate for checking type, otherwise check if VAL equals TYPE."
  (when types
    (unless (seq-some (lambda (type)
                        ;; if type is a function assume it is predicate
                        (or (and (symbolp type)
                                 (functionp type)
                                 (funcall type val))
                            ;; otherwise check if value = type
                            (equal type val)))
                      types)
      (error "The value of '%s' is '%s' and it does not fit to specified types/values: '%s'" key val types))))

(defvar org-catch---processed-store nil
  "Variable for storing evaluated results for processed keywords to be used/bound when ':eval-bind' specification is set.")

(defun org-catch--process (method data &optional multiple-ok keywords)
  "Looks up keyword(s) for KWD-IS in `org-catch-keywords' and gets those from DATA. Check if values have correct types (if ':types' is specified for these keywords).
If ':eval' is specified in `org-catch-keywords' for this keywords do the evaluation and return results otherwise return value.
With MULTIPLE-OK do it for all matching keywords otherwise if more that one keyword was matched throw and error (i.e., there should be only one keyword for KWD-IS ''target').
KEYWORDS if not provided defaults to `org-catch-keywords'"
  (when-let ((keywords (or keywords org-catch-keywords
                           (message "No keywords defined!")))
             ;; find keywords specs for 'method'
             (method-kwds (or (map-filter
                               (lambda (key val) (equal method (plist-get val :method)))
                               keywords)
                              (and (message "No keywords found for method '%s'." method) nil)))
             ((or multiple-ok
                  (= (map-length method-kwds) 1)
                  (error "Multiple keywords are not expected for method '%s'" method)))
             ;; get data for 'method'
             (method-data
              (map-apply
               (lambda (kwd spec)
                 (let* ((kwd-data
                         (if-let ((kwd-name (symbol-name kwd))
                                  ((string-search "*" kwd-name))
                                  (kwd-re (org-catch---glob-re kwd-name)))
                             ;; logic to differentiate between match keywords by glob patterns
                             ;; the idea is that some keywords can be globed by the other
                             ;; but others are not!
                             (let* ((other-kwds
                                     (mapcar 'symbol-name (remove kwd (map-keys keywords))))
                                    (other-kwds-globable
                                     (seq-filter (lambda (o) (string-match-p kwd-re o)) other-kwds))
                                    (other-kwds-globable-re
                                     (mapconcat 'org-catch---glob-re other-kwds-globable "\\|")))
                               (map-filter
                                (lambda (key val)
                                  (setq key-name (symbol-name key))
                                  (and (string-match-p kwd-re key-name)
                                       ;; either there is no other keywords that can be globed
                                       ;; or non of these subglobable other keywords are matched
                                       (or (not other-kwds-globable)
                                           (not (string-match-p other-kwds-globable-re key-name)))))
                                data))
                           (when (plist-member data kwd)
                             ;; same format as returned by map-filter
                             `((,kwd . ,(plist-get data kwd)))))))
                   ;; attach :data to spec
                   (cons kwd (plist-put spec :data kwd-data))))
               method-kwds)))
    ;; check types
    (map-do
     (lambda (kwd spec)
       (when-let ((kwd-types (plist-get spec :types))
                  (kwd-data (plist-get spec :data)))
         (map-do
          (lambda (k v) (org-catch---check-type k v kwd-types))
          kwd-data)))
     method-data)
    ;; evaluation with bindings
    (let ((method-eval-results
           (map-values-apply
            (lambda (spec)
              (let* ((kwd-data (plist-get spec :data))
                     (eval-bind (plist-get spec :eval-bind))
                     (eval-bindings
                      (when eval-bind
                        ;; check type
                        (unless (and (listp eval-bind)
                                     (seq-every-p 'symbolp eval-bind))
                          (error ":eval-bind should be of list of :method symbols"))
                        ;; make alist from all results
                        (seq-mapcat
                         (lambda (_method-data)
                           ;; flatten all the data
                           ;; somewhat check if it is bindable
                           (when-let (((proper-list-p _method-data))
                                      ((seq-every-p 'listp _method-data))
                                      (_method-data
                                       (seq-mapcat 'identity _method-data))
                                      ((seq-every-p 'listp _method-data)))
                             (seq-mapcat 'identity _method-data)))
                         ;; filter results to bind
                         (map-values
                          (map-filter
                           (lambda (_method _)
                             (member _method eval-bind))
                           org-catch---processed-store))))))
                ;; evaluations
                (if-let ((eval-form (plist-get spec :eval)))
                    ;; eval form with lexical bindings
                    (map-apply
                     (lambda (key val)
                       (eval eval-form
                             (append `((_val . ,val)
                                       (_key . ,key)
                                       ;; assume key is keywordp
                                       (_kwd . ,(substring (symbol-name key) 1)))
                                     ;; add bindings from :eval-bind
                                     eval-bindings)))
                     kwd-data)
                  ;; if :eval-bind is provided without :eval evaluate value directly with bindings
                  (if eval-bind
                      (map-values-apply
                       (lambda (val) (eval val eval-bindings))
                       kwd-data)
                    ;; otherwise return raw values
                    (map-values kwd-data)))
                ))
            method-data)))
      ;; store data to bind in following evaluations
      (push `(,method . ,method-eval-results) org-catch---processed-store)
      ;; return value if it is single value method
      (if multiple-ok
          method-eval-results
        (caar method-eval-results)))))

;;;; --------------
;;;; main interface
;;;; --------------

(defun org-catch (template &optional keywords arg)
  (let* ((arg (or arg current-prefix-arg))
         (data (org-catch--data template arg))
         (target (org-catch--process 'target data nil keywords))
         (target-buffer
          (pcase target
            ('nil (current-buffer))
            ((pred bufferp) target)
            ((pred markerp) (marker-buffer target))))
         (target-point
          (pcase target
            ('nil (point))
            ((pred markerp) target)))
         (datetree (org-catch--process 'target-datetree data nil keywords)))
    (setq org-catch---processed-store nil)
    ;; initial evaluations
    (org-catch--process 'eval-init data 'multiple-ok keywords)
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
          ;; evaluate before new item
          (org-catch--process 'eval-before data 'multiple-ok keywords)
          ;; create new heading and set target-point to bol
          (when-let ((item (org-catch--process 'target-item data nil keywords)))
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
          (when-let ((body (org-catch--process 'body data nil keywords)))
            (save-excursion
              ;; for inserting props and stuff with body
              (when item (setq body (concat "\n" (string-trim body) "\n\n")))
              (insert body)))
          ;; write all org keywords and properties if it is an entry
          (when (and target-point
                     (goto-char target-point)
                     (not (org-before-first-heading-p)))
            (org-catch--process 'set-properties data 'multiple-ok keywords))
          ;; eval at target after everything is inserted
          (org-catch--process 'eval-after data 'multiple-ok keywords)
          ;; save the buffer
          (save-buffer))))
    ;; do all the edits and inserts back in the initial context
    (org-catch--process 'eval-final data 'multiple-ok keywords)))





;; test single kwd

;; (org-catch
;;  '(:single-kwd-two-methods t)
;;  nil
;;  '(:single-kwd-two-methods (:method eval-init
;;                             :types (t plistp)
;;                             :eval (when _val
;;                                     `((_file . ,(buffer-file-name))
;;                                       (_intro . "File: "))))
;;    :single-kwd-two-methods (:method eval-final
;;                             :types (t plistp)
;;                             :eval-bind (eval-init)
;;                             :eval (when _val
;;                                     (insert _intro)
;;                                     (insert _file)))))



;;;; -------
;;;; provide
;;;; -------

(provide 'org-catch)

;; org-catch.el ends here
;; --------<<  engine:1 ends here


