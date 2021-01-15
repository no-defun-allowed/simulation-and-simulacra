(in-package :simulation-and-simulacra)

(defvar *history* (make-array 8192 :fill-pointer 0 :adjustable t)) 

(defun simulation-loop (&key (jobs 200))
  (setf (fill-pointer *history*) 0)
  (let ((best-rods   (list 0 0))
        (best-pearls (list 0 0))
        (iterations 0)
        (alien-stuff (make-alien-stuff jobs)))
    (loop
      (multiple-value-bind (rod-rods rod-pearls pearl-rods pearl-pearls)
          (run-test :alien-stuff alien-stuff)
        (incf iterations (* jobs +kernel-iterations+))
        (setf best-rods
              (multiple-value-call #'list
                (maximize-rods rod-rods rod-pearls
                               (first best-rods) (second best-rods))))
        (setf best-pearls
              (multiple-value-call #'list
                (maximize-pearls pearl-rods pearl-pearls
                                 (first best-pearls) (second best-pearls))))
        (format t "~&~5,3,2e iterations: (~{~3d rods, ~2d~}) (~{~3d, ~2d pearls~})"
                iterations best-rods best-pearls)
        (format t "    this run: (~3d rods, ~2d) (~3d, ~2d pearls)"
                rod-rods rod-pearls pearl-rods pearl-pearls)
        (vector-push-extend `(,iterations ,@best-rods
                                          ,@best-pearls
                                          ,rod-rods ,rod-pearls
                                          ,pearl-rods ,pearl-pearls)
                            *history*)
        (when (or (and (>= rod-pearls 42)
                       (>= rod-rods   211))
                  (and (>= pearl-pearls 42)
                       (>= pearl-rods   211)))
          (format t "~& you win!!!!!1!!!!one!")
          (return))))))
