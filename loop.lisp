(in-package :simulation-and-simulacra)

(defconstant +kernel-iterations+ 10000
  "The value of ITERATIONS from simulate.cl")

(defvar *si-prefixes* " kMGBTPEZY")
(defun format-short-si-prefix (stream n &optional colon? at?)
  (declare (ignore colon? at?))
  (let* ((unit-position (floor (log (* n 100) 1000)))
         (normalized-n  (/ n (expt 1000 unit-position))))
    (format stream "~5,3f" normalized-n)
    (write-char (char *si-prefixes* unit-position) stream)))

(defun simulation-loop (&key (jobs 8192))
  (let ((best-rods 0)
        (best-pearls 0)
        (iterations 0))
    (loop
      (multiple-value-bind (this-rods this-pearls)
          (run-test :jobs jobs)
        (incf iterations (* jobs +kernel-iterations+))
        (alexandria:maxf best-rods this-rods)
        (alexandria:maxf best-pearls this-pearls)
        (format t "~&~/simulation-and-simulacra::format-short-si-prefix/ iterations: ~3d rods, ~2d pearl trades"
                iterations best-rods best-pearls))
      (when (and (>= best-pearls 42)
                 (>= best-rods   211))
        (format t "~& you win!!!!!1!!!!one!")
        (return)))))
