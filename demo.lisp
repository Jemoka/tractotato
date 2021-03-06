;; Some compilers (e.g. SBCL) fail to reload the system with `defpackage' when
;; exports are spread around.  `uiop:define-package' does not have this problem.

(in-package :cl-user)
(uiop:define-package tractotato-demo
                     (:use :cl :tractotato))
(in-package :tractotato-demo)

(defparameter *testdb* 
  (entry 
    "this"
    (end 12)
    (tags ("tag1" "tag2" ("tag3" (weight 2))))
    (children 
      ("child0"
       ("child1" (sequential t))
       ("child2" 
        (children
          (("childchild1"))))
       ("child3" (end 192012))))))

(index *testdb* (search "ta" title) :for entry :with tags)

