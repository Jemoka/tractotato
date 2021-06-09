(in-package :tractotato)

(defstruct project
  "A project"
  (title ""))

(defstruct tag
  "A tag"
  (title ""))

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

(defmacro track (title &body body)
  "Track time"

  (let ((entries 
          (mapcan (lambda (n) 
                    (cond
                      ((eq (car n) 'project) `(:project (make-project :title ,(cadr n))))
                      (t `(,(intern (symbol-name (car n)) "KEYWORD") ,(cadr n))))
                    ) body)))
    `(make-entry 
       :title ,title
       ,@entries)))
  ;(let (vars)
    ;(progn
      ;(if (member 's body) (push '(s :start) vars))
      ;(if (member 'e body) (push '(e :end) vars))
      ;(if (member 'p body) (push '(p :project) vars))
      ;(if (member 'tg body) (push '(tg :tags) vars))
      ;(if (member 'tl body) (push '(tl :title) vars))
      ;(if (member 'r body) (push '(r :running) vars)))
    ;`(let 
       ;,vars
       ;(make-entry ,@body))))

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

