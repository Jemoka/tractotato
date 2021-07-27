(in-package :tractotato)

;; TODO janky AF but eh
;; https://stackoverflow.com/questions/15710108/merging-symbols-in-common-lisp
(defun symbol-append (&rest symbols) 
  (intern (apply #'concatenate 'string 
                 (mapcar #'symbol-name symbols))))

(defmacro entry-index (body var &key (collect-rule 'or))
  (let ((n (gensym)) (k (gensym)))
    (cond
      ((eq var 'project) 
       `(lambda (,n) (funcall ,body (project-title (entry-project ,n)))))
      ((eq var 'tags)
       `(lambda (,n) (eval (macroexpand (cons (quote ,collect-rule) (mapcar (lambda (,k) 
                                                                              (funcall ,body (tag-title ,k) (tag-weight ,k))) (entry-tags ,n)))))))
      (t `(lambda (,n) 
            (,(car body) ,(cadr body) (,(symbol-append 'entry- var) ,n))))))) 
(export 'entry-index)

