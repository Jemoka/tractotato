(in-package :tractotato)

(defvar *tracto-reserved-keywords*
  (mapcar #'closer-mop:slot-definition-name
          (append 
            (closer-mop:class-direct-slots (find-class 'tag))
            (closer-mop:class-direct-slots (find-class 'entry))
            (closer-mop:class-direct-slots (find-class 'tracto)))))

(export '*tracto-reserved-keywords*)

(defun match-prop (key entry)
  (remove-if #'null 
             (cons 
               (if (funcall key entry)
                   entry)
               (mapcan (lambda (new)
                         (match-prop key new))
                       (children entry)))))

(defun match-tags (key entry)
  (append
    (remove-if (lambda (n) 
                 (not (funcall key n)))
               (slot-value entry 'tags))
    (mapcan (lambda (new)
              (match-tags key new))
            (children entry))))

(defun process-key (key item)
  (mapcar (lambda (n)
            (cond 
              ;; TODO this is bad but eh
              ((and (symbolp n) 
                    (member (symbol-name n) 
                            (mapcar #'symbol-name 
                            *tracto-reserved-keywords*)
                            :test #'string=)) 
               (list (intern (symbol-name n) :tractotato) 
                     item))
              ((listp n) (process-key n item))
              (t n))) key))

(defmacro index (entry by &key (for 'entry) (with 'statement))
  (let* ((n (gensym))
         (target-tags (gensym))
         (key `(lambda (,n)
                 ,(process-key by n))))
    (cond 
      ((and (eql for 'entry) 
            (eql with 'tags)) 
       `(let ((,target-tags
                (match-tags
                  ,key ,entry)))
          (match-prop (lambda (,n) 
                        (intersection 
                          (tags ,n) ,target-tags)) 
                      ,entry)))
      ((and (eql for 'tags)
            (eql with 'statement))
       `(match-tags ,key ,entry))
      ((and (eql for 'entry)
            (eql with 'statement))
       `(match-prop ,key ,entry)))))

(export 'index)

