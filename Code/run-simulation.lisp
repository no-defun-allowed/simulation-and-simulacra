(in-package :simulation-and-simulacra)

(defun randomize (vector)
  (dotimes (n (length vector))
    (setf (aref vector n) (random (expt 2 64)))))

(defun make-random-u64-vector (size)
  "Make a random input vector, to seed the RNGs of the workers."
  (let ((vector (make-array size :element-type '(unsigned-byte 64))))
    (randomize vector)
    vector))

;;; Nothing!
(defun reload ())

(defstruct (alien-stuff (:constructor %make-alien-stuff)))

(defun make-alien-stuff (jobs)
  (declare (ignore jobs))
  (%make-alien-stuff))

(defconstant +rod-limit+ 211)
(defconstant +pearl-limit+ 42)

(declaim (inline maximize-rods maximize-pearls))
(defun maximize-rods (rod-rods rod-pearls best-rod-rods best-rod-pearls)
  (if (or (and (< best-rod-rods +rod-limit+)
               (> rod-rods best-rod-rods))
          (and (>= rod-rods +rod-limit+)
               (> rod-pearls best-rod-pearls)))
      (values rod-rods      rod-pearls)
      (values best-rod-rods best-rod-pearls)))
(defun maximize-pearls (pearl-rods pearl-pearls best-pearl-rods best-pearl-pearls)
  (if (or (and (< best-pearl-pearls +pearl-limit+)
               (> pearl-pearls best-pearl-pearls))
          (and (>= pearl-pearls +pearl-limit+)
               (> pearl-rods best-pearl-rods)))
      (values pearl-rods      pearl-pearls)
      (values best-pearl-rods best-pearl-pearls)))

(defun read-off-results (results)
  (let ((best-rod-rods     0)
        (best-rod-pearls   0)
        (best-pearl-rods   0)
        (best-pearl-pearls 0))
    (declare ((unsigned-byte 64)
              best-rod-rods best-rod-pearls
              best-pearl-rods best-pearl-pearls))
    (loop for result in results
          for rod-rods     = (aref result 0)
          for rod-pearls   = (aref result 1)
          for pearl-rods   = (aref result 2)
          for pearl-pearls = (aref result 3)
          do (multiple-value-setq (best-rod-rods best-rod-pearls)
               (maximize-rods rod-rods rod-pearls
                              best-rod-rods best-rod-pearls))
             (multiple-value-setq (best-pearl-rods best-pearl-pearls)
               (maximize-pearls pearl-rods pearl-pearls
                                best-pearl-rods best-pearl-pearls)))
    (values best-rod-rods best-rod-pearls
            best-pearl-rods best-pearl-pearls)))

(defun run-test (&key (jobs 200)
                      (alien-stuff (make-alien-stuff jobs)))
  (declare (ignore alien-stuff))
  (read-off-results (lparallel:pmapcar #'simulate
                                       (make-random-u64-vector jobs))))
