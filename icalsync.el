(defgroup icalsync nil
  "Options concerning iCalSync."
  :tag "iCalSync")

(defcustom icalsync-el-incomplete-p-function 'icalsync-el-incomplete-todo-p
  "The function used to determine if a TODO is incomplete.
    
    The function should take a single argument, TODO-STATE,
    consisting of the keyword of the TODO item."
  :type 'symbol
  :group 'icalsync)

(defcustom icalsync-el-valid-p-function 'icalsync-el-valid-todo-p
  "The function used to determine if a TODO should be exported.
    
The function should take two arguments; TODO-STATE, a string
consisting of the keyword of the TODO item, and TODO-TEXT, a
string consisting of the TODO item's headline text."
  :type 'symbol
  :group 'icalsync)

(defcustom icalsync-el-todo-collection-function 'org-all-todos
  "The function used to collect all TODO items from a buffer.
    
The function should take an argument; BUFFER, a buffer name or
buffer object, and return a list of plists, each representing a
TODO item / iCal task. See org-all-todos for details."
  :type 'symbol
  :group 'icalsync)

(defun icalsync-capture-ccl-result (command)
  "Convert a result from the SLIME Lisp's REPL into the Emacs
Lisp toplevel."
  (read (slime-eval `(swank::pprint-eval ,command))))

(defun icalsync-choose-calendar (calendar-name)
  "Choose a calendar, and return the UID of the calendar chosen."
  (interactive (list (completing-read "Complete: " (icalsync-calendar-names-from-mapping))))
  (gethash calendar-name (icalsync-calendar-name->uid-mapping)))

(defun icalsync-calendar-name->uid-mapping ()
  "Return a hash table with keys of calendar names pointing to
iCal UIDs.

Expects a connection to Clozure CL and the CalendarStore
Framework in Mac OS X Lion."
  (let ((capture (icalsync-capture-ccl-result "(icalsync-cl-calendar-names-and-uids)"))
        (result (make-hash-table :test 'equal)))
    (loop for (title . uid) in capture
          do (puthash title uid result))
    result))

(defun icalsync-calendar-names-from-mapping ()
  "Return a list of calendar names."
  (let (names)
    (maphash (lambda (name uid)
               (setq names (cons name names)))
             (icalsync-calendar-name->uid-mapping))
    names))

;; (icalsync-el-sync-org->ical "inbox.org")

;; TODO(msnyder): Use org-heading-components

(defun icalsync-el-sync-org->ical (buffer)
  "Turn an org mode buffer into a list of iCal tasks.

A function that describes one way to retrieve all of the valid
TODOs from a buffer for turning into respective iCal tasks. You
can write a function that best suits your workflow. The function
is expected to take the name of a buffer, or a buffer, and return
a list of plists, each one representing a TODO item / iCal task."
  (save-excursion
    (switch-to-buffer buffer)
    (beginning-of-buffer)
    (while (re-search-forward org-todo-line-regexp nil :return-t-if-found-and-nil-otherwise)
      (let ((headline (match-string-no-properties 0))
            (todo-state (match-string-no-properties 2))
            (todo-text (match-string-no-properties 3))
            (scheduled-time (org-get-scheduled-time (point)))
            todo-uid)
        (if (and todo-state
                 (funcall icalsync-el-valid-p-function todo-state todo-text))
            (progn
              (if (string-match org-todo-line-tags-regexp headline)
                  (setq headline (icalsync-el-remove-metadata headline (list :tags nil
                                                                             :todo-keyword nil
                                                                             :priority nil))))
              (if (string-match "^*+" headline)
                  (setq headline (replace-match "" t t headline)))
              
              (setq todo-uid
                    (icalsync-capture-ccl-result (format "(icalsync-cl-create-task '%S)"
                                                         (list
                                                          :calendar-uid (icalsync-el-calendar-name)
                                                          :incomplete-p (funcall icalsync-el-incomplete-p-function todo-state)
                                                          :title (org-trim headline)
                                                          :scheduled (and scheduled-time (decode-time scheduled-time))))))
              
              (goto-char (car (org-get-property-block nil nil t)))
              (org-set-property "ICALSYNC-UID" todo-uid)))))))

(defun icalsync-el-calendar-exists-p (calendar-name)
  (find calendar-name (icalsync-calendar-names-from-mapping)
        :test 'string=))

(defun icalsync-el-calendar-name ()
  "Find a calendar name for the task in the current buffer.

If we can't find a valid calendar name, ask the user through
icalsync-choose-calendar."
  (or
   (icalsync-choose-calendar (org-get-category))   (call-interactively 'icalsync-choose-calendar)))

(defun icalsync-el-incomplete-todo-p (todo-state)
  (string= todo-state "TODO"))

(defun icalsync-el-valid-todo-p (todo-state todo-text)
  (string= todo-state "TODO"))

(defun icalsync-el-remove-metadata (string opts)
  (let ((re org-complex-heading-regexp)
        (todo (plist-get opts :todo-keywords))
        (tags (plist-get opts :tags))
        (pri  (plist-get opts :priority))
        (elts '(1 2 3 4 5))
        (case-fold-search nil)
        rpl)
    (setq elts (delq nil (list 1 (if todo 2) (if pri 3) 4 (if tags 5))))
    (when (or (not todo) (not tags) (not pri))
      (if (string-match re string)
          (mapconcat (lambda (i) (if (match-end i) (match-string i string) "")) elts " ")))))
