(require 'org-catch)
(require 'org-catch-helpers)

;; first define some common properties for new entry
(defvar org-catch-created-properties-tempate
  '(:created (org-current-time-as-inactive-timestamp-string)
    :created-on-system (system-name)
    :created-by-user (user-login-name)
    :created-while-at
    (org-with-wide-buffer
     ;; if filing the subtree then store link to context
     (and (org-at-heading-p 'invisible-not-ok)
          (stringp :item)
          (equal (org-get-heading t t t t) :item)
          (org-up-heading-or-point-min))
     (when-let ((link (org-store-link nil)))
       (substring-no-properties link))))
  "Default properties that logs context for newly created org entries.")

;; catch things to journal
(defvar org-catch-default-journal "~/org/journal.org")

(defun org-catch-journal (&optional arg)
  "Creates a new item in the `org-catch-default-journal' under datetree (see `org-datetree.el').

With interactive ARG first ask for a date for datetree where the new journal entry should be filed. Otherwise file the entry for current date.

Then it asks user for org tags unless it is called when the point is at heading in which case the current heading's tags will be used.

Then
- If called with active region refile this region and ask for title
- If called while on org subtree heading refile this subtree
- If called at plain text refile the paragraph at point and ask for title
- Otherwise ask user for title and body of the new journal record

At the end it will delete the text that was refiled and insert the back reference link.

The new journal entry will also have properties to log some context. See `org-catch-created-properties-tempate'."
  (interactive "p")
  (org-catch
   `(:target org-catch-default-journal
     :datetree ((1 . (or region-time t))
                (4 . read-time))
     :tags (or at-header-tags (read-multi nil ":" "note" "idea" "meeting"))
     :item (or (and (not region) at-header) read)
     :body (or region at-header-body paragraph read)
     :edit (or delete-region
               delete-at-header-subtree
               delete-paragraph)
     :backref (make-link :item)
     ,@org-catch-created-properties-tempate)
   arg))

;; catch todos
(defun org-catch-todo (arg)
  "Catch a new TODO entry with. When called with interactive ARG prefix consider `org-agenda-files' for filing targets. Otherwise seek targets in current buffer.

First asks user for filing target. Consider as targets only entries that does not have a todo keyword or has 'PROJ' as todo keyword to avoid nested TODOs.

Then asks for tags.

Then asks for title for the new TODO entry unless:
- if there is active region use this region as title
- if the point is at heading then refile the subtree and make it a TODO
- if the point is as org list item then use it as a TODO and refile sub item elements if any as TODO body
- otherwise just ask user for a TODO title

At the end delete used text and insert back reference at point."
  (interactive "p")
  (org-catch
   `(:target ((1 . (read-ol 'buffer (todo-p nil "PROJ")))
              (4 . (read-ol nil (todo-p nil "PROJ"))))
     :tags (or header-at-tags (read-multi nil ":" "@home" "@office" "@city"))
     :item (or region header-at list-item read)
     :body (or header-at-body list-body)
     :todo "TODO"
     :edit (or delete-region
               delete-at-header-subtree
               (and list-body delete-list)
               delete-list-item)
     :backref (make-link :item)
     ,@org-catch-created-properties-tempate)
   arg))

;; org util
(require 'cl-macs) ; provides cl-letf*
(defun org-todo-done (&optional arg)
  "Set TODO entry as done. With ARG ask when it was done and record it accordingly."
  (interactive "P")
  (let ((todo-fun (if (derived-mode-p 'org-agenda-mode) 'org-agenda-todo 'org-todo)))
    (if arg
        (cl-letf* ((time (org-read-date 'with-time 'to-time nil "When this was done? "))
                   ((symbol-function 'org-current-effective-time) #'(lambda () time))
                   ((symbol-function 'org-today) #'(lambda () (time-to-days time))))
          (print (org-current-effective-time))
          (print (org-today))
          (funcall todo-fun 'done))
      (funcall todo-fun 'done))))

;; set todo as done
(defun org-catch-done (arg)
  "Ask for a target which is any todo entries in current `org-agenda-files' and set this entry as done. With interactive prefix ARG also ask when it was done.

At the end insert the back reference wrapped as +[[org-id][item]]+, i.e., wrapped in strike-through org markup."
  (interactive "p")
  (org-catch
   `(:target (read-ol nil todo-p)
     :funcall ((1 . 'org-todo-done)
               (4 . '(org-todo-done arg)))
     :backref (make-link :item nil "+"))
   arg))

(provide 'org-catch-examples)
