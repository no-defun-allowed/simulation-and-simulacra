(in-package :simulation-and-simulacra)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun binomial-pmf (k n p)
    (* (alexandria:binomial-coefficient n k)
       (expt p k)
       (expt (- 1 p) (- n k))))
  
  (defun binomial-cdf (minimum n p)
    (loop for k from minimum to n
          sum (binomial-pmf k n p)))
  
  (defun binomial-table (n p &key (maximum n))
    (loop with one = (binomial-cdf 0 n p)
          for k from 0 to maximum
          for x = (min (floor (* (expt 2 64)
                                 (/ (binomial-cdf k n p) one)))
                       (1- (expt 2 64)))
          unless (zerop x)
            collect x)))

(defun print-binomial-table (n p
                             &key (variable-name "binomial_table")
                                  (length-name "BINOMIAL_TABLE_LENGTH"))
  (let ((table (append (binomial-table n p :maximum 64) (list 0))))
    (format t "~&__constant unsigned long ~a[] = {~%~{  0x~16,'0x~^, 0x~16,'0x~^, 0x~16,'0x~^, 0x~16,'0x~^,~%~}~%};"
            variable-name
            table)
    (format t "~&#define ~a ~d" length-name (length table))))

(alexandria:define-constant +binomial-table+
    (coerce (append (binomial-table 262 (/ 20 423)
                                    :maximum 64)
                    '(0))
            '(vector (unsigned-byte 64)))
  :test #'equalp)
(defconstant +binomial-table-length+ (length +binomial-table+))
