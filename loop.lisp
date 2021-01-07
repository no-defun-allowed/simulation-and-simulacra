(in-package :simulation-and-simulacra)

(defconstant +kernel-iterations+ 40000
  "The value of ITERATIONS from simulate.cl")

(defun simulation-loop (&key (jobs 8192))
  (let ((best-rods   (list 0 0))
        (best-pearls (list 0 0))
        (iterations 0)
        (alien-stuff (make-alien-stuff jobs)))
    (loop
      (handler-case
          (run-test :alien-stuff alien-stuff)
        (eazy-opencl.bindings:opencl-error (e)
          (cond
            ((eql (eazy-opencl.bindings:opencl-error-code e)
                  'eazy-opencl.bindings:out-of-host-memory)
             (format t "~&ow ow ow I'm GCing now")
             (trivial-garbage:gc :full t))
            (t
             (error e))))
        (:no-error (rod-rods rod-pearls pearl-rods pearl-pearls)
          (incf iterations (* jobs +kernel-iterations+))
          (maximize (rod-rods   (first best-rods)
                     rod-pearls (second best-rods))
            best-rods (list rod-rods rod-pearls))
          (maximize (pearl-pearls (second best-pearls)
                     pearl-rods   (first best-pearls))
            best-pearls (list pearl-rods pearl-pearls))
          (format t "~&~5,3,2e iterations: (~{~3d rods, ~2d~}) (~{~3d, ~2d pearls~})"
                  iterations best-rods best-pearls)
          (format t "~&            this run: (~3d rods, ~2d) (~3d, ~2d pearls)"
                  rod-rods rod-pearls pearl-rods pearl-pearls)
          (when (or (and (>= rod-pearls 42)
                         (>= rod-rods   211))
                    (and (>= pearl-pearls 42)
                         (>= pearl-rods   211)))
            (format t "~& you win!!!!!1!!!!one!")
            (return)))))))
