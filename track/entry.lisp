(in-package :tractotato)

(defstruct project
  "A project"
  (title ""))

(defstruct tag
  "A tag"
  (title "")
  (weight 1))

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

(defmacro transform-tags (tags)
  "Transform tag entries"

  (mapcar (lambda (n)
            (if (atom n) 
                `(make-tag :title ,n) 
                `(make-tag ,@n))) tags))

(defmacro track (title &body body)
  "Track time"

  (let ((entries 
          (mapcan (lambda (n) 
                    (cond
                      ((eq (car n) 'project) `(:project (make-project :title ,(cadr n))))
                      ((eq (car n) 'tags) `(:tags `(,(transform-tags (cadr n)))))
                      (t `(,(intern (symbol-name (car n)) "KEYWORD") ,(cadr n))))
                    ) body)))
    `(make-entry 
       :title ,title
       ,@entries)))

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

