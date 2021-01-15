(in-package :simulation-and-simulacra)

(defun list-devices (platform n)
  (declare (ignore platform n))
  (write-line "any number: your CPU")
  (values))

(defun list-platforms (&optional (*standard-output* *standard-output*))
  (list-devices 0 0)
  (values))

(defun choose-device (cores)
  (setf lparallel:*kernel* (lparallel:make-kernel cores)))
