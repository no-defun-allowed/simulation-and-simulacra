(defconstant +multiplier+ #x5deece6dd)
(defconstant +addend+ #xb)
(defvar *seed* (random (expt 2 48)))

(defun next-value ()
  (setf *seed*
        (ldb (byte 48 0) (+ +addend+ (* +multiplier+ *seed*)))))

(defun sample (low high)
  (< (mod (next-value) high) low))
