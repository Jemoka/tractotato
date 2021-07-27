; vi:syntax=lisp

(defsystem "tractotato"
  :description "A potato that tracks"
  :version "0.1"
  :author "Houjun Liu"
  :licence "MIT License"
  :depends-on ( "closer-mop" )
  :components ((:file "package")
               (:module track
                    :components 
                    ((:file "entry")
                     (:file "indexing")
                     (:file "track")))
                (:module analyze
                    :components ((:file "analyze")))))

