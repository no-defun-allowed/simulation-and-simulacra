(asdf:defsystem :simulation-and-simulacra
  :depends-on (:alexandria :lparallel)
  :components ((:file "package")
               (:file "device")
               (:file "run-simulation")
               (:file "simulate")
               (:file "loop")))
