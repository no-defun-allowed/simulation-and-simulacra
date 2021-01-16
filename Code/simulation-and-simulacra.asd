(asdf:defsystem :simulation-and-simulacra
  :depends-on (:alexandria :lparallel)
  :components ((:file "package")
               (:file "device")
               (:file "generate-cdf-table")
               (:file "run-simulation")
               (:file "simulate")
               (:file "loop")))
