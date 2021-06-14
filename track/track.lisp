(in-package :tractotato)

;; TODO janky AF but eh
;; https://stackoverflow.com/questions/15710108/merging-symbols-in-common-lisp
(defun symbol-append (&rest symbols) 
  (intern (apply #'concatenate 'string 
                 (mapcar #'symbol-name symbols))))

(defmacro index (body)
  (cond
    ((eq (cadr body) 'project) `(lambda (n) (,(car body) (project-title (entry-project n)) ,(caddr body))))
    (t `(lambda (n) 
          (,(car body) (,(symbol-append 'entry- (cadr body)) n) ,(caddr body))))))

(index (search project "chicken"))


(list (track "A good time entry"
        (project "Eating")
        (tags ("aftercare"
               "transition"))) 

      (track "Another time entry"
        (project "Swimming")
        (tags ("one"
               "two")))) 



