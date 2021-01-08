(ql:quickload :alexandria)

(defun binomial-pmf (k n p)
  (* (alexandria:binomial-coefficient n k)
     (expt p k)
     (expt (- 1 p) (- n k))))

(defun binomial-cdf (minimum n p)
  (loop for k from minimum to n
        sum (binomial-pmf k n p)))

(defun binomial-table (n p)
  (loop with one = (binomial-cdf 0 n p)
        for k from 0 to n
        for x = (min (floor (* (expt 2 64)
                               (/ (binomial-cdf k n p) one)))
                     (1- (expt 2 64)))
        unless (zerop x)
          collect x))

(defun print-binomial-table (n p
                             &key (variable-name "binomial_table")
                                  (length-name "BINOMIAL_TABLE_LENGTH"))
  (let ((table (append (binomial-table n p) (list 0))))
    (format t "~&__constant unsigned long ~a[] = {~%~{  0x~16,'0x~^, 0x~16,'0x~^, 0x~16,'0x~^, 0x~16,'0x~^,~%~}~%};"
            variable-name
            table)
    (format t "~&#define ~a ~d" length-name (length table))))

;; (print-binomial-table 262 (float (/ 20 423) 1.0d0))
