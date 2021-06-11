(in-package :tractotato)

(defmacro query (title &body body)
  "Get a time entry query function"
  ())

(list (track "A good time entry"
        (project "Eating")
        (tags ("aftercare"
               "transition"))) 

      (track "Another time entry"
        (project "Swimming")
        (tags ("one"
               "two")))) 


(stop-entry v)

