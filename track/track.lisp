(in-package :tractotato)

(defstruct store
  "A datastore"
  (name "")
  ())

(defparameter *active-store* nil)
(defparameter *active-store-data* nil)


