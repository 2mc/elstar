(require 'objc-support)
(require 'nsclasp)

(objc:load-framework "CalendarStore" :calendarstore)

(defun icalsync-cl-coerce-null-pointer (value)
  "Return nil if VALUE is a null pointer, and VALUE otherwise."
  (if (and (ccl::pointerp value) (%null-ptr-p value)) nil
      value))

(defun icalsync-cl-calendar-store ()
  "Return the calendar store. Equivalent to
    [CalCalendarStore defaultCalendarStore]."
  (#/defaultCalendarStore ns:cal-calendar-store))

(defun icalsync-cl-all-calendars ()
  "Return a list of all calendars in the default calendar
    store. Equivalent to [[CalCalendarStore defaultCalendarStore]
    calendars]."
  (#/calendars (icalsync-cl-calendar-store)))

(defun icalsync-cl-calendar-names-and-uids ()
  "Return a Lisp list of cons cells, consisting of the names and UIDs
    of each calendar in the default calendar store."
  (let ((calendars (icalsync-cl-all-calendars)))
    (loop for i upto (1- (#/count calendars))
       for calendar = (#/objectAtIndex: calendars i)
       collect (cons
                (ccl::lisp-string-from-nsstring (#/title calendar))
                (ccl::lisp-string-from-nsstring (#/uid calendar))))))

(defvar *task-properties*
  (list "title"
        "uid"
        "priority"
        "dueDate"
        "dateStamp"
        "completedDate"))

(defun icalsync-cl-get-task (uid)
  (let ((task (#/taskWithUID: (icalsync-cl-calendar-store) (ccl::%make-nsstring uid))))
    (if (%null-ptr-p task)
        nil
        task)))

(defun icalsync-cl-coerce-value (value)
  "Convert VALUE into a native Lisp type of some kind. The conversions
    supported are: NSString to lisp string, NSDate to encoded universal
    time, and null pointer to nil."
  (cond
    ((nsclasp:ns-string-p value)
     (ccl::lisp-string-from-nsstring value))
    ((nsclasp:ns-date-p value)
     (nsclasp:ns-date->encoded-universal-time value))
    (t (icalsync-cl-coerce-null-pointer value))))

(defun icalsync-cl-task->plist (task)
  "Convert a CalTask to a plist of properties. The list of properties
    is described by *task-properties*."
  (loop
     for property in *task-properties*
     for value = (funcall (intern property "NEXTSTEP-FUNCTIONS") task)
     append (list (intern (format nil "~:@(~a~)" property)) (icalsync-cl-coerce-value value))))

(defun icalsync-cl-create-task (task-plist)
  (let ((title (getf task-plist :title))
        (calendar-uid (getf task-plist :calendar-uid))
        (incomplete-p (getf task-plist :incomplete-p))
        (scheduled (getf task-plist :scheduled))
        (task (#/task ns:cal-task))
        (calendar-store (icalsync-cl-calendar-store)))
    (setf (#/title task) (ccl::%make-nsstring title))
    (setf (#/calendar task) (#/calendarWithUID: calendar-store
                                                (ccl::%make-nsstring calendar-uid)))
    (setf (#/isCompleted task) (not incomplete-p))
    (if scheduled
        (setf (#/dueDate task) (nsclasp:time->ns-date scheduled)))
    
    (#/saveTask:error: calendar-store task (%null-ptr))
    (list :uid (icalsync-cl-coerce-value (#/uid task))
          :dt (icalsync-cl-coerce-value (#/dateStamp task)))))
