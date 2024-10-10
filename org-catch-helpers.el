;; -------->>  [[file:org-catch.src.org::*helpers][helpers:1]]
(require 'org-catch)

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


(defmacro org-catch---with-prev-item (&rest body)
  "Eval BODY with P1 and P2 vars (markers) bind for previous list item boundaries."
  `(save-excursion
     (when-let (((org-at-item-p))
                ((let (org-list-use-circular-motion)
                   (ignore-errors (org-previous-item))))
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
       ,@body)))


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


(defun org-catch--helper-prev-item (&optional no-time)
  "If in org list gets previous list item as string. With NO-TIME arg remove time prefix if present (e.g., 5:30 or 4:20-6:00)."
  (org-catch---with-prev-item
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

;; references
(defun org-catch--helper-ref ()
  "Return reference to current point as :file, :item, :id  plist"
  `(:file ,(buffer-file-name)
    :item ,(when (derived-mode-p 'org-mode)
             (unless (org-before-first-heading-p)
               (org-get-heading t t t t)))
    :id ,(when (derived-mode-p 'org-mode)
           (unless (org-before-first-heading-p)
             (org-id-get-create)))))

(defun org-catch--helper-match-time (str)
  "Retur substring that matches time of the day or time interval."
  (when (and (stringp str)
             (string-match org-catch--helper--time-prefix-re str))
    (match-string 1 str)))

(defun org-catch--helper-link (&rest spec)
  "Return org link formated according to key value arguments (SPEC).  If :file and :id SPEC is not provided use current context as reference."
  (let ((spec
         (if (or (plist-get spec :file)
                 (plist-get spec :id))
             spec
           (append spec (org-catch--helper-ref)))))
    (concat
     ;; prefix
     (when-let ((prefix (plist-get spec :prefix)))
       (concat prefix " "))
     ;; org link (wrapped)
     (plist-get spec :wrap) "[["
     (or (when-let ((id (plist-get spec :id)))
           (concat
            (when-let ((type (plist-get spec :type)))
              (concat type "-"))
            "id:" id))
         (when-let ((file (plist-get spec :file)))
           (concat "file:" file)))
     "]"
     ;; link text
     (when-let (text (or (plist-get spec :text)
                         (plist-get spec :item)))
       (concat "[" text "]"))
     "]" (plist-get spec :wrap)
     ;; suffix
     (when-let ((suffix (plist-get spec :suffix)))
       (concat " " suffix)))))


;; (org-catch--helper-link :type "prev")

(defun org-catch--helper-log (fmt &rest spec)
  "Format string FMT with the rest of :KEYWORD VALUE arguments as format specifications. Note that only first letter of :KEYWORD is taken for specification.
Some preset specifications are:
  %l - link to current org entry or file
  %t - current timestamp in org format."
  (let* ((time (format-time-string "%Y-%m-%d %a %H:%M" (current-time)))
         (link (or (apply 'org-catch--helper-link (org-catch--helper-ref)) ""))
         (spec (map-apply (lambda (key val)
                            (cons
                             (string-to-char
                              (string-replace ":" "" (symbol-name key)))
                             val))
                          spec)))
    (format-spec fmt (append spec `((?t . ,time) (?l . ,link))))))

;; (org-catch--helper-log "%l on [%t] by %i" :iam "user")


(defun org-catch--helper-logbook (fmt &rest spec)
  "Insert log (see `org-catch--helper-log') to current logbook."
  (save-excursion
    (goto-char (org-log-beginning 'create))
    (insert (apply 'org-catch--helper-log fmt spec) "\n")))


(defun org-catch--helper-find-id (str)
  "Look up org id in the STR and get marker if it is found."
  (when-let (((string-match org-link-bracket-re str))
             (id (match-string 1 str))
             ((string-match "id:\\(.*\\)\\'" id)))
    (org-id-find (match-string 1 id) 'markerp)))

;; tags

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
  "If region is active delete it."
  (org-with-wide-buffer
   (when-let (((use-region-p))
              (p1 (region-beginning))
              (p2 (region-end)))
     (delete-region p1 p2)
     (goto-char p1))))

(defun org-catch--helper-delete-paragraph ()
  "If at paragraph (i.e. text surrounded by two new lines or by org headers) delete this paragraph."
  (unless (or
           ;; do not work on heading-or-item
           (and (derived-mode-p 'org-mode)
                (org-at-heading-or-item-p))
           ;; check if we are on the text line
           (and (looking-at-p "[ \t]*\n")
                (looking-back "\n[ \t]*")))
    (org-with-wide-buffer
     (org-catch---with-paragraph
      (delete-region p1 p2)
      (goto-char p1)))))

;; (funcall (org-catch--helper-delete-paragraph))

(defun org-catch--helper-delete-list-item ()
  (when (and (org-at-item-p) (org-in-item-p))
    (org-catch---with-list-item
     (org-with-wide-buffer
      (delete-region p1 p2)
      (goto-char p1)))))


(defun org-catch--helper-delete-list ()
  (when (and (org-at-item-p) (org-in-item-p))
    (org-catch---with-list
     (org-with-wide-buffer
      (delete-region p1 p2)
      (goto-char p1)))))

(defun org-catch--helper-delete-at-header-subtree ()
  (when-let (((org-at-heading-p 'invisible-not-ok))
             (p1 (save-excursion
                   (org-back-to-heading)
                   (point)))
             (p2 (org-with-wide-buffer
                  (org-end-of-subtree 'invisible-ok)
                  (point))))
    (org-with-wide-buffer
     (delete-region p1 p2)
     (goto-char p1))))


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

(defun org-catch--helper-read-ol (&rest specs)
  "Reads org outline path and returns a marker. The agruments are keyword value with the following :KEYWORDs and VALUES:
:PROMT string to use as a promt
:TARGETS keyword value can be either or a list of the folloing:
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
:FILTER keyword value (basically any predicate function but here are some helpers sortcuts `org-catch--helper-'):
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
:INPUT keyword value:
  STRING                  - default input string
  region                  - use current region as default input"
  (let* ((prompt (or (plist-get specs :prompt)
                     org-catch---key-prompt
                     "org-catch--helper-read-ol: "))
         (targets (plist-get specs :targets))
         (org-refile-targets
          (if targets
              (if (or (when (or (symbolp targets)
                                (list-of-strings-p targets))
                        ;; convert to list for mapcar
                        (setq targets (list targets)))
                      (when (stringp targets)
                        ;; confert to double list
                        (setq targets (list (list targets))))
                      (proper-list-p targets))
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
                              (`(,_targets . (,_key . ,_val)) f)
                              (_ (error "Bad :TARGETS value specification"))))
                          targets)
                (error "Bad FILES arg specification"))
            ;; if files nil use current defauls
            org-refile-targets))
         ;; elements filtering
         (filter (plist-get specs :filter))
         (org-refile-target-verify-function filter)
         ;; advice fucntion for injecting devault input in refile dialog
         ;; args of `completing-read'
         ;; prompt collection &optional predicate require-match initial-input hist def inherit-input-method
         ;; 0      1                    2         3             4
         ;;                                                     ^ it is 4th argument
         (input (plist-get specs :input))
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
                                 (with-current-buffer (find-file-noselect target-file)
                                   (org-with-wide-buffer (point-max)))))
                 (m (make-marker)))
        (set-marker m target-pos (find-file-noselect target-file))
        ;; clear cache if we use filters because
        (when filter (org-refile-cache-clear))
        ;; return marker
        m))))

;; (org-catch--helper-read-ol :targets 'buffer :filter (org-catch--helper-subtree-p))
;; (org-catch--helper-read-ol :targets (org-catch--helper-read-agenda-set)     :filter (org-catch--helper-todo-p))
;; (org-catch--helper-read-ol :targets (org-catch--helper-agenda-set "orgzly") :filter (org-catch--helper-todo-p) (org-catch--helper-region))
;; (org-catch--helper-read-ol :targets (org-catch--helper-agenda-set "orgzly") :filter (org-catch--helper-todo-p) (org-catch--helper-region))




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

(provide 'org-catch-helpers)

;; org-catch-helpers.el ends here
;; --------<<  helpers:1 ends here


