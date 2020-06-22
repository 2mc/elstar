;;; icalsync.el -- Emacs Lisp code for Elstar Calendar

;; Copyright (C) 2012 Matthew Snyder

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA or see <http://www.gnu.org/licenses/gpl-2.0.txt>

;; Author: Matthew Snyder <matthew.c.snyder@gmail.com>
;; URL: http://github.com/ardekantur/elstar
;; Version: 0.0.1

;; This file is not part of GNU Emacs.

;;; Developer Notes:

;; This could all be a little cleaner. At the end of the day, there
;; should really be one configurable function, that takes a buffer or
;; a list of buffers, and returns a list of structures that contain
;; the calendar names and TODO structures retrieved from those
;; buffers.

(require 'cl)

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

(defun icalsync-el-transform-todos (buffer)
  "Turn an org mode buffer into a list of iCal tasks.

A function that describes one way to retrieve all of the valid
TODOs from a buffer for turning into respective iCal tasks. You
can write a function that best suits your workflow. The function
is expected to take the name of a buffer, or a buffer, and return
a list of plists, each one representing a TODO item / iCal task.

FIXME(msnyder): Use org-map-entries to make this easier."
  (save-excursion
    (let ((calendar-uid (icalsync-el-calendar-name)) todos)
      (switch-to-buffer buffer)
      (beginning-of-buffer)
      (while (re-search-forward org-todo-line-regexp nil t)
        (let* ((todo-text (match-string-no-properties 3))
               (components (org-heading-components))
               (todo-state (nth 2 components))
               (headline (nth 4 components))
               (scheduled-time (org-get-scheduled-time (point)))
               todo-uid)

          (if (and todo-state
                   (funcall icalsync-el-valid-p-function todo-state todo-text))
              (progn
                (if (string-match org-todo-line-tags-regexp headline)
                    (setq headline (nth 4 (org-heading-components)))
                  (setq todos (cons (list
                                     :buffer (buffer-file-name)
                                     :point (point)
                                     :calendar-uid calendar-uid
                                     :incomplete-p (funcall icalsync-el-incomplete-p-function todo-state)
                                     :title (org-trim headline)
                                     :scheduled (and scheduled-time (decode-time scheduled-time))
                                     :uid (cdr (assoc "ICALSYNC-UID" (org-entry-properties)))
                                     :dt (and
                                          (cdr (assoc "ICALSYNC-DT" (org-entry-properties)))
                                          (read (cdr (assoc "ICALSYNC-DT" (org-entry-properties)))))) todos)))))))
      todos)))

(defun icalsync-el-upload-todo (todo)
  (let (result)
    (save-excursion
      (setq result
            (icalsync-capture-ccl-result
             (format "(icalsync-cl-create-task '%S)" todo)))
      (find-file (getf todo :buffer))
      (goto-char (getf todo :point))
      (goto-char (car (org-get-property-block nil nil t)))
      (org-set-property "ICALSYNC-UID" (getf result :UID))
      (org-set-property "ICALSYNC-DT" (format "%S" (getf result :DT))))))

(defun icalsync-el-download-todo (todo)
  (save-excursion
    (find-file (getf todo :buffer))
    (goto-char (getf todo :point))
    (let* ((result (icalsync-capture-ccl-result
                   (format "(icalsync-cl-task->plist (icalsync-cl-get-task %S))" (getf todo :uid))))
           (t1 (apply 'encode-time (read (cdr (assoc "ICALSYNC-DT" (org-entry-properties))))))
           (t2 (apply 'encode-time (getf result 'DATESTAMP))))
      (if (time-less-p t1 t2)
          (progn
            (if (getf result 'COMPLETEDDATE)
                (org-todo 'done))
            (org-set-property "ICALSYNC-DT" (format "%S" t2)))))))

(defun icalsync-el-upload-todos (todos)
  (loop for todo in todos
        do (icalsync-el-upload-todo todo)))

(defun icalsync-el-download-todos (todos)
  (loop for todo in todos
        do (icalsync-el-download-todo todo)))

(defun icalsync-el-calendar-exists-p (calendar-name)
  (find calendar-name (icalsync-calendar-names-from-mapping)
        :test 'string=))

(defun icalsync-el-calendar-name ()
  "Find a calendar name for the task in the current buffer.

If we can't find a valid calendar name, ask the user through
icalsync-choose-calendar."
  (or
   (icalsync-choose-calendar (org-get-category))
   (call-interactively 'icalsync-choose-calendar)))

(defun icalsync-el-incomplete-todo-p (todo-state)
  (string= todo-state "TODO"))

(defun icalsync-el-valid-todo-p (todo-state todo-text)
  (string= todo-state "TODO"))

(provide 'icalsync)

;;; icalsync.el ends here
