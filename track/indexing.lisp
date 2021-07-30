(in-package :tractotato)


(defparameter *test* (entry "haa"
                            (end 12091312)
                            (children ("this" 
                                       ("that"
                                        (end 19230123)
                                        (children ("this" 
                                                   ("that"))))))))

(defun match (property target entry &key (key #'equal))
  (remove-if #'null 
             (cons 
               (if (funcall key (slot-value entry property) target)
                   entry)
               (mapcan (lambda (new)
                         (match property target new :key key))
                       (children entry)))))

(match 'title "FISHBLUBBER" *test*)



;(defgeneric match (entry index &key (recurse t))
;  )



;;; TODO janky AF but eh
;;; https://stackoverflow.com/questions/15710108/merging-symbols-in-common-lisp
;(defun symbol-append (&rest symbols) 
  ;(intern (apply #'concatenate 'string 
                 ;(mapcar #'symbol-name symbols))))

;(defmacro entry-index (body var &key (collect-rule 'or))
  ;(let ((n (gensym)) (k (gensym)))
    ;(cond
      ;((eq var 'project) 
       ;`(lambda (,n) (funcall ,body (project-title (entry-project ,n)))))
      ;((eq var 'tags)
       ;`(lambda (,n) (eval (macroexpand (cons (quote ,collect-rule) (mapcar (lambda (,k) 
                                                                              ;(funcall ,body (tag-title ,k) (tag-weight ,k))) (entry-tags ,n)))))))
      ;(t `(lambda (,n) 
            ;(,(car body) ,(cadr body) (,(symbol-append 'entry- var) ,n)))))))


