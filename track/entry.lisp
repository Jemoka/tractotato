(in-package :tractotato)

(defstruct entry
  "A time tracking entry"
  (start (get-universal-time))
  (end nil)
  (project nil)
  (tags nil)
  (title "")
  (running t))

(export '(entry 
          entry-end
          entry-project
          entry-tags
          entry-title
          entry-running))

(defun continue-entry (entry)
  "Destructively continue the time entry"
  (progn 
    (setf (entry-end entry) nil)
    (setf (entry-running entry) t)
    entry))
(export 'continue-entry)

(defun stop-entry (entry)
  "Destructively stop the time entry"
  (progn 
    (setf (entry-end entry) (get-universal-time))
    (setf (entry-running entry) nil)
    entry))
(export 'stop-entry)

