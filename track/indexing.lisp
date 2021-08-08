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
                              (list (slot-value entry property) target)
                              (list target (slot-value entry property))))
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
                              `(,(slot-value n property) ,target)
                              `(,target ,(slot-value n property))))))
               (slot-value entry 'tags))
    (mapcan (lambda (new)
              (match-tags key property 
                          target new 
                          :direction direction))
            (children entry))))

(defmacro index (entry with &key (for 'entry) (by 'statement))
  (let ((target-tags (gensym))
        (key (car with))
        (target
          (car 
            (remove-if (lambda (n) 
                         (member n *tracto-reserved-keywords*)) 
                       (cdr with))))
        (property
          (car 
            (remove-if (lambda (n) 
                         (not (member n (cons (car with) *tracto-reserved-keywords*)))) (cdr with)))))
    (let ((direction (if (eql (cadr with) property) :ltr :rtl))) 
      `(cond 
         ((and (eql ',for 'entry) 
               (eql ',by 'tags)) 
          (let ((,target-tags
                  (match-tags
                    ,key ,property ,target
                    ,entry)))
            (match-prop #'intersection 'tags ,target-tags ,entry)))
         ((and (eql ',for 'tags)
               (eql ',by 'statement))
          (match-tags #',key ',property ,target ,entry :direction ,direction))
         ((and (eql ',for 'entry)
               (eql ',by 'statement))
          (match-prop #',key ',property ,target ,entry :direction ,direction))))))

(match-tags #'< 'weight 12 *hewo* :direction :rtl)

;(index *hewo* (< 12 weight) :for tags)
;(macroexpand '(index *hewo* :with (eql title "ha")))
;(macroexpand '(index *hewo* (< 12 weight) :for tags))

;(start *hewo*)

;(defparameter *hewo* 
  ;(entry "fush"
         ;(tags ("hi"))
         ;(children 
           ;("fish" "bish" "rishfish" 
            ;("nish" 
             ;(tags ("hibeba" "nish" ("hi" (weight 12))))
             ;(children ("fish")))))))

;;;(index *hewo*
   ;;;:for tag
   ;;;:by tag 
   ;;;:with (string= title "hi"))

