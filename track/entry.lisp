(in-package :tractotato)

(setf *random-state* (make-random-state t))

(defclass tracto ()
  ((id
     :initform (+ (random 999999999) 100000000)
     :initarg :id
     :reader id
     :documentation "id of the tracto object")
   (title 
     :initform ""
     :initarg :title
     :accessor title
     :documentation "title of the tracto object")))
(export '(id title))

(defclass tag (tracto)
  ((weight
     :initform 1
     :initarg :weight
     :documentation "weight of the tag")))
(defmacro make-tag (&body body)
  `(make-instance 'tag ,@body))
(export '(make-tag))

(defclass entry (tracto)
  ((start
     :initform (get-universal-time)
     :initarg :start
     :accessor start
     :documentation "start time of entry")
   (end
     :initform nil
     :initarg :end
     :accessor end
     :documentation "end time of entry")
   (children
     :initform nil
     :initarg :children
     :accessor children
     :documentation "the children of entry")
   (sequential
     :initform nil 
     :initarg :sequential
     :accessor sequential
     :documentation "sequentiality of the entry")
   (running
     :initform t
     :initarg :running
     :accessor running
     :documentation "whether the entry is running")
   (tags
     :initform nil 
     :initarg :tags
     :accessor tags
     :documentation "tags of the entry")))
(defmacro make-entry (&body body)
  `(make-instance 'entry ,@body))
(export '(make-entry))


(defgeneric weight (obj)
  (:documentation "get weight")
  (:method (obj)
   (format t "don't think that object has weights~%")))

(defmethod weight ((obj tag))
  (slot-value obj 'weight))

(defmethod weight ((obj entry))
  (+ (apply #'* (mapcar 
               (lambda (n) (slot-value n 'weight)) 
               (tags obj)))
     (apply #'+ (mapcar 
               (lambda (n) (weight n)) 
               (children obj)))))

(defmethod (setf weight) (new-value (obj tag))
  (setf (slot-value obj 'weight) new-value))

(defmacro transform-tags (&body body)
  "Transform tag entries"
  `(list ,@(mapcar (lambda (n)
                     (if (atom n) 
                         `(make-tag :title ,n) 
                         `(make-tag ,@n))) body)))

(defmacro track (title &body body)
  "Track time"
  (let ((entries 
          (mapcan (lambda (n) 
                    (cond
                      ((eq (car n) 'tags) `(:tags ,(macroexpand (cons 'transform-tags (cadr n)))))
                      ((eq (car n) 'children) `(:children (list ,@(mapcar (lambda (e) (macroexpand (cons 'track e))) (cadr n)))))
                      (t `(,(intern (symbol-name (car n)) "KEYWORD") ,(cadr n))))
                    ) body)))
    `(make-entry 
       :title ,title
       ,@entries)))


(macroexpand '(track "this"
                (end 12)
                (tags ("tag1" "tag2"))
                (children 
                  (("that" (end 12) (sequential t))
                   ("that" (end 12))
                   ("that" (end 12))))))

(export 'track)

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

