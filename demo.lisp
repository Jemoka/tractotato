;; Some compilers (e.g. SBCL) fail to reload the system with `defpackage' when
;; exports are spread around.  `uiop:define-package' does not have this problem.

(in-package :cl)
(uiop:define-package tractotato-demo
                     (:use :cl :tractotato))
(in-package :tractotato-demo)

(in-package :cl-user)

;(weight )
(tractotato:title (caddr (tractotato:tags (eval (cons 'tractotato:entry (tractotato:serialize  (tractotato:entry "this"
                                                                 (end 12)
                                                                 (tags ("tag1" "tag2" ("tag3" (weight 2))))
                                                                 (children 
                                                                   (("child1" (sequential t))
                                                                    ("child2" 
                                                                     (children
                                                                       (("childchild1"))))
                                                                    ("child3" (end 192012)))))))))))     

