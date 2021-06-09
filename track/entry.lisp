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
      (if (member 'st body) (push '(st :start) vars))
      (if (member 'en body) (push '(en :end) vars))
      (if (member 'pj body) (push '(pj :project) vars))
      (if (member 'tg body) (push '(tg :tags) vars))
      (if (member 'tt body) (push '(tt :title) vars))
      (if (member 'rn body) (push '(pj :running) vars)))
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

