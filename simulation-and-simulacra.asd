(asdf:defsystem :simulation-and-simulacra
  :depends-on (:eazy-opencl)
  :components ((:file "package")
               (:file "device")
               (:file "run-simulation")))
