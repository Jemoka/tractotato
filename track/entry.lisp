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

(export 'weight)

(defmacro transform-tags (&body body)
  "Transform tag entries"
  `(list ,@(mapcar (lambda (n)
                     (if (atom n) 
                         `(make-tag :title ,n) 
                         `(make-tag ,@n))) body)))

(export 'transform-tags)

(defmacro parse (title &body body)
  "Parse entry"
  (let ((entries 
          (mapcan (lambda (n) 
                    (cond
                      ((eq (car n) 'tractotato:tags) 
                       `(:tags ,(macroexpand (cons 'tractotato:transform-tags (cadr n)))))
                      ((eq (car n) 'tractotato:children) `(:children (list ,@(mapcar (lambda (e) (macroexpand (cons 'tractotato:parse e))) (cadr n)))))
                      (t `(,(intern (symbol-name (car n)) "KEYWORD") ,(cadr n))))
                    ) body)))
    `(make-entry 
       :title ,title
       ,@entries)))

(defmacro entry (&body body)
  "Overload parse as 'entry'"
  `(tractotato:parse ,@body))

(export '(parse 
           entry))

(defun flatten (l)
  "Flatten!"
  ;; https://stackoverflow.com/questions/2680864/how-to-remove-nested-parentheses-in-lisp
  (cond ((null l) nil)
        ((atom l) (list l))
        (t (loop for a in l appending (flatten a)))))

(defgeneric serialize (obj)
  (:documentation "serialize an object")
  (:method (obj)
   (format t "don't think that object is an entry that could be serialized~%"))
  (:method ((obj entry))
   `(,(title obj)
      ,@(mapcar
          (lambda (slot)
            (let ((slot-name (closer-mop:slot-definition-name slot)))
              (cond 
                ((eq slot-name 'tags) 
                 (list 'tags (let ((tgs (tags obj)))
                               (mapcar (lambda (tg)
                                         (flatten (mapcar 
                                                    (lambda (tsl)
                                                      (let ((tsl-symb (closer-mop:slot-definition-name tsl)))
                                                        (list (intern (symbol-name tsl-symb) "KEYWORD") (slot-value tg tsl-symb))))
                                                    (closer-mop:class-slots (find-class 'tag))))) 
                                       tgs))))
                ((eq slot-name 'children)
                 (list 'children (mapcar (lambda (ent) (serialize ent)) (children obj))))
                (t (list slot-name (slot-value obj slot-name)))
                )))
          (closer-mop:class-slots (find-class 'entry))))))

(export '(serialize tags children))

