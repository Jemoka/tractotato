(in-package :tractotato)

;; TODO janky AF but eh
;; https://stackoverflow.com/questions/15710108/merging-symbols-in-common-lisp
(defun symbol-append (&rest symbols) 
  (intern (apply #'concatenate 'string 
                 (mapcar #'symbol-name symbols))))

(defmacro index (body &key (collect-rule 'or))
  (let ((n (gensym)) (k (gensym)))
    (cond
    ((eq (cadr body) 'project) 
         `(lambda (,n) (funcall ,(car body) (project-title (entry-project ,n)))))
    ((eq (cadr body) 'tags)
         `(lambda (,n) (macroexpand (cons (quote ,collect-rule) (mapcar (lambda (,k) 
                                       (funcall ,(car body) (tag-title ,k) (tag-weight ,k))) (entry-tags ,n))))))
    (t `(lambda (,n) 
          (,(car body) ,(cadr body) (,(symbol-append 'entry- (caddr body)) ,n))))))) 
(export 'index)

;(defun filter-entries ())
;(index ((lambda (name weight) (search name "chicken")) tags))
;(index (search start "chicken"))


;(macroexpand '(index (search title "good"))) 

;(funcall (index (search "good" title)) (track "A good time entry"
                                         ;(project "Eating")
                                         ;(tags ("aftercare"
                                                ;"transition")))) 

(remove-if-not (index (< 12 start)) (list (track "A good time entry"
                                                     (project "Eating")
                                                     (tags ("aftercare"
                                                            "transition"))) 

                                                   (track "Another time entry"
                                                     (project "Swimming")
                                                     (tags ("one"
                                                            "two")))))  



