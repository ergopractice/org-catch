`org-catch` is a concise and flexible alternative to `org-capture`. It allows to set the [order of user input](#input-order) precisely. For example, `org-catch` can ask user to choose a filing target before asking for a title of new org entry, whereas `org-capture` allows to refile an entry to a specific target only after the entry is fully defined and it requires and extra command to call.

`org-catch` also provides a handful of [helper functions](#helpers) that can be used as shortcuts within a template. This makes `org-catch` template neat and concise. See [examples](#examples).


# Installation

The package is not yet on melpa so you can get it with straight.el, for example, by adding the following to your `init.el`:

```emacs-lisp
(use-package org-catch
  :straight (:host github :repo "ergopractice/org-catch"))
```

Otherwise, download it manually (change the YOUR-ELISP-DIR to the directory path you would like to store `org-catch` on your computer):

```shell
cd YOUR-ELISP-DIR
git clone https://github.com/ergopractice/org-catch.git
```

Then add the following to your `init.el`:

```emacs-lisp
(load-path YOUR-ELISP-DIR/org-catch)
(require 'org-catch)
```

As of now the package is under ongoing development, so, please, pull it regularly (e.g., whith `straight-pull-package`). Your [comments and suggestions](https://github.com/ergopractice/org-catch/issues) are very welcome and helpful.


<a id="input-order"></a>

# Order of user input

The key feature of `org-catch` is that order in which user inputs required information for new record creation or edit follows the the template precisely. For example:

```emacs-lisp
;; insert new TODO item at point

;; first ask for tags and then for title
(org-catch
 '(:tags (completing-read-multiple "Tags: " '("@home" "@office" "@city"))
   :item (read-string "Title: ")
   :todo "TODO"))

;; first title for tags and then for tags
(org-catch
 '(:item (read-string "Title: ")
   :tags (completing-read-multiple "Tags: " '("@home" "@office" "@city"))
   :todo "TODO"))
```


<a id="helpers"></a>

# Helpers

`org-catch` also provides handful of helper functions that provide convenience of user interaction and content creation. Helpers functions match `org-catch--helper-*` prefix (by default set in `org-catch--prefix` variable) and it can be used for defining templates as shortcuts, i.e. without the prefix. Helpers shortcuts can be used even without wrapping it as function calls. For example, the three `org-catch` templates below are equivalent:

```emacs-lisp
;; helpers are not enabled by default
(require 'org-catch-helpers)

;; Helper functions as shortcut variables (the most concise varian)
(org-catch
 '(:target read-ol
   :tags (read-multi nil ":" "@home" "@office" "@city")
   :item (or list-item read)
   :todo "TODO"))

;; Helper functions just as function shortcuts
(org-catch
 '(:target (read-ol)
   :tags (read-multi nil ":" "@home" "@office" "@city")
   :item (or (list-item) (read))
   :todo "TODO"))

;; Direct usage of helper functions (same as for any other elisp)
(org-catch
 '(:target (org-catch--helper-read-ol)
   :tags (org-catch--helper-read-multi nil ":" "@home" "@office" "@city")
   :item (or (org-catch--helper-list-item)
             (org-catch--helper-read))
   :todo "TODO"))
```


# Keywords

All `org-catch` keywords are specified in `org-catch-keywords` variable. User can change any keyword to own liking and extend `org-catch` with extra functionalities. Each keyword is associated with a method which is called in the following workflow:

-   Eval things while at initial context
    -   `eval-init`
-   Get target and go there
    -   `target`
    -   `target-datetree`
-   Eval at target before inserting new org entry
    -   `eval-before` (binds results from `eval-init`)
-   Insert new org entry, body and properties
    -   `target-item`
    -   `target-body`
    -   `set-properties`
-   Eval at target after making new org entry
    -   `eval-after` (binds results from `eval-init` and `eval-before`)
-   Go back to initial context and eval final things
    -   `eval-final` (binds results from `eval-init`, `eval-before` and `eval-after` )


<a id="examples"></a>

# Examples

Below are some example user commands which documentation that hopefully explains its org-catch templates. These examples are included in the package. Add `(require 'org-catch-examples)` to your `init.el` to try them out. Note that you might also want to set `org-catch-default-journal` variable beforehand.

```emacs-lisp
;; first define some common properties for new entry
(defvar org-catch-created-properties-tempate
  '(:created (org-current-time-as-inactive-timestamp-string)
    :created-on-system (system-name)
    :created-by-user (user-login-name)
    :created-while-at
    (org-with-wide-buffer
     ;; if filing the subtree then store link to context
     (and (org-at-heading-p 'invisible-not-ok)
          (stringp _item)
          (equal (org-get-heading t t t t) _item)
          (org-up-heading-or-point-min))
     (when-let ((link (org-store-link nil)))
       (substring-no-properties link))))
  "Default properties that logs context for newly created org entries.")

;; catch things to journal
(defvar org-catch-default-journal "~/org/journal.org")

(defun org-catch-journal ()
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
  (interactive)
  (org-catch
   `(:target org-catch-default-journal
     :datetree ((1 . (or region-time t))
                (4 . read-time))
     :tags (or at-header-tags (read-multi nil ":" "note" "idea" "meeting"))
     :item (or (and (not region) at-header) read)
     :body (or region at-header-body paragraph read)
     :final '(or delete-region
                 delete-at-header-subtree
                 delete-paragraph)
     :insert-ref t
     ,@org-catch-created-properties-tempate)))

;; catch todos
(defun org-catch-todo ()
  "Catch a new TODO entry with. When called with interactive ARG prefix consider `org-agenda-files' for filing targets. Otherwise seek targets in current buffer.

First asks user for filing target. Consider as targets only entries that does not have a todo keyword or has 'PROJ' as todo keyword to avoid nested TODOs.

Then asks for tags.

Then asks for title for the new TODO entry unless:
- if there is active region use this region as title
- if the point is at heading then refile the subtree and make it a TODO
- if the point is as org list item then use it as a TODO and refile sub item elements if any as TODO body
- otherwise just ask user for a TODO title

At the end delete used text and insert back reference at point."
  (interactive)
  (org-catch
   `(:target ((1 . (read-ol :targets 'buffer :filter (todo-p nil "PROJ")))
              (4 . (read-ol :filter (todo-p nil "PROJ"))))
     :tags (or header-at-tags (read-multi nil ":" "@home" "@office" "@city"))
     :item (or region header-at list-item read)
     :body (or header-at-body list-body)
     :todo "TODO"
     :final '(or delete-region
                 delete-at-header-subtree
                 (and list-body delete-list)
                 delete-list-item)
     :insert-ref t
     ,@org-catch-created-properties-tempate)))

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
(defun org-catch-done ()
  "Ask for a target which is any todo entries in current `org-agenda-files' and set this entry as done. With interactive prefix ARG also ask when it was done.

At the end insert the back reference wrapped as +[[org-id][item]]+, i.e., wrapped in strike-through org markup."
  (interactive)
  (org-catch
   `(:target (read-ol :filter todo-p)
     :before ((1 . '(org-todo-done))
              (4 . '(org-todo-done arg)))
     :insert-ref '(:wrap "+"))))
```
