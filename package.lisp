;; Some compilers (e.g. SBCL) fail to reload the system with `defpackage' when
;; exports are spread around.  `uiop:define-package' does not have this problem.

(uiop:define-package tractotato 
                     (:use :cl))

