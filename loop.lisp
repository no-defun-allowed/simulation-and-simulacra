(in-package :simulation-and-simulacra)

(defconstant +kernel-iterations+ 10000
  "The value of ITERATIONS from simulate.cl")

(defun simulation-loop (&key (jobs 8192))
  (let ((best-rods 0)
        (best-pearls 0)
        (iterations 0))
    (loop
      (handler-case
          (run-test :jobs jobs)
        (eazy-opencl.bindings:opencl-error ()
          (trivial-garbage:gc :full t))
        (:no-error (this-rods this-pearls)
          (incf iterations (* jobs +kernel-iterations+))
          (alexandria:maxf best-rods this-rods)
          (alexandria:maxf best-pearls this-pearls)
          (format t "~&~5,3e iterations: ~3d rods, ~2d pearl trades"
                  iterations best-rods best-pearls)))
      (when (and (>= best-pearls 42)
                 (>= best-rods   211))
        (format t "~& you win!!!!!1!!!!one!")
        (return)))))
