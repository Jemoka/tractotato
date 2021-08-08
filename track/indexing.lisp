(in-package :tractotato)

(defvar *tracto-reserved-keywords*
  (mapcar #'closer-mop:slot-definition-name
          (append 
            (closer-mop:class-direct-slots (find-class 'tag))
            (closer-mop:class-direct-slots (find-class 'entry))
            (closer-mop:class-direct-slots (find-class 'tracto))))) 

(defun match-prop (key property target entry &key (direction :ltr))
  (remove-if #'null 
             (cons 
               (if (apply key 
                          (if (equal direction :ltr)
                              (list (funcall property entry) target)
                              (list target (funcall property entry))))
                   entry)
               (mapcan (lambda (new)
                         (match-prop key property 
                                     target new 
                                     :direction direction))
                       (children entry)))))

(defun match-tags (key property target entry &key (direction :ltr))
  (append
    (remove-if (lambda (n) 
                 (not (apply key 
                          (if (equal direction :ltr)
                              `(,(funcall property n) ,target)
                              `(,target ,(funcall property n))))))
               (slot-value entry 'tags))
    (mapcan (lambda (new)
              (match-tags key property 
                          target new 
                          :direction direction))
            (children entry))))

(defmacro index (entry &key by (for 'entry) (with 'statement))
  (let ((target-tags (gensym))
        (key (car by))
        (target
          (car 
            (remove-if (lambda (n) 
                         (member n *tracto-reserved-keywords*)) 
                       (cdr by))))
        (property
          (car 
            (remove-if (lambda (n) 
                         (not (member n (cons (car by) *tracto-reserved-keywords*)))) (cdr by)))))
    (let ((direction (if (eql (cadr by) property) :ltr :rtl))) 
      (cond 
         ((and (eql for 'entry) 
               (eql with 'tags)) 
          `(let ((,target-tags
                  (match-tags
                    #',key #',property ,target
                    ,entry :direction ,direction)))
            (match-prop #'intersection #'tags ,target-tags ,entry)))
         ((and (eql for 'tags)
               (eql with 'statement))
          `(match-tags #',key #',property ,target ,entry :direction ,direction))
         ((and (eql for 'entry)
               (eql with 'statement))
          `(match-prop #',key #',property ,target ,entry :direction ,direction))))))

