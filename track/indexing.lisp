(in-package :tractotato)

(defparameter *test* (entry "this"
                            (end 12)
                            (tags ("tag1" "tag2" ("tag3" (weight 2))))
                            (children 
                              ("child0"
                               ("child1" (sequential t))
                               ("child2" 
                                (children
                                  (("childchild1"))))
                               ("child3" (end 192012))))))

(defun match-prop (statement entry)
  (let ((key (car statement))
        (property (cadr statement))
        (target (caddr statement)))
    (remove-if #'null 
               (cons 
                 (if (funcall key (slot-value entry property) target)
                     entry) ;entry
                 (mapcan (lambda (new)
                           (match-prop statement new))
                         (children entry))))))

(defun match-tags (statement entry)
  (let ((key (car statement))
        (property (cadr statement))
        (target (caddr statement)))
    (append
      (remove-if (lambda (n) 
                   (not (funcall key 
                                 (slot-value n property)
                                 target)))
                 (slot-value entry 'tags))
      (mapcan (lambda (new)
                (match-tags statement new))
              (children entry)))))



;(defun tags-match)
(match-tags 
  '(string= title "hi") 
  (entry "FUSH"
         (tags ("hi"))
         (children 
           ("FISH" "BISH" "RISHFISH" 
            ("NISH" 
             (tags ("hibeba" "nish" ("hi" (weight 12))))
             (children ("FISH")))))))


