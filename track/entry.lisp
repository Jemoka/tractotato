(in-package :tractotato)

(defstruct entry
  "A time tracking entry"
  (start (get-universal-time))
  (end nil)
  (project nil)
  (tags nil)
  (title "")
  (running t))
(export '(entry-end
          entry-project
          entry-tags
          entry-title
          entry-running))

(defmacro entry (&body body)
  "Track time"

  (let (vars)
    (progn
      (if (member 's body) (push '(s :start) vars))
      (if (member 'e body) (push '(e :end) vars))
      (if (member 'p body) (push '(p :project) vars))
      (if (member 'tg body) (push '(tg :tags) vars))
      (if (member 'tl body) (push '(tl :title) vars))
      (if (member 'r body) (push '(r :running) vars)))
    `(let 
       ,vars
       (make-entry ,@body))))

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

