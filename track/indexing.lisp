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
                              (list (slot-value n property) target)
                              (list target (slot-value n property))) 
                          entry)))
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
               (eql ',by 'tag)) 
          (let ((,target-tags
                  (match-tags
                    ,key ,property ,target
                    ,entry)))
            (match-prop #'intersection 'tags ,target-tags ,entry)))
         ((and (eql ',for 'tag)
               (eql ',by 'statement))
          (match-tags #',key ',property ,target ,entry :direction ,direction))
         ((and (eql ',for 'entry)
               (eql ',by 'statement))
          (match-prop #',key ',property ,target ,entry :direction ,direction))))))

;(macroexpand '(index *hewo* :with (eql title "ha")))
(index *hewo* (< 12 start))
(index *hewo* (> start 12))
;(start *hewo*)

;(defparameter *hewo* 
  ;(entry "FUSH"
         ;(tags ("hi"))
         ;(children 
           ;("FISH" "BISH" "RISHFISH" 
            ;("NISH" 
             ;(tags ("hibeba" "nish" ("hi" (weight 12))))
             ;(children ("FISH")))))))

;;;(index *hewo*
   ;;;:for tag
   ;;;:by tag 
   ;;;:with (string= title "hi"))

