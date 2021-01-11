(in-package :simulation-and-simulacra)

(defun plot (&key (pathname #p"/tmp/plot.png") (history *history*))
  (eazy-gnuplot:with-plots (*standard-output* :debug t)
    (eazy-gnuplot:gp-setup :xlabel "Iterations"
                           :ylabel "Value"
                           :output pathname
                           :terminal :png
                           :key '(:top :right)
                           :yrange '#:|[0:250]|) ; wtf
    (loop for n from 1 below (length (aref history 0))
          for name in (alexandria:map-product (lambda (&rest r)
                                                (format nil "~{~a~^ ~}" r))
                                              '("Best" "This")
                                              '("rod" "pearl")
                                              '("run")
                                              '("rods" "pearls"))
          for colour in '("red" "red"       "purple" "purple"
                          "orange" "orange" "blue" "blue")
          do (eazy-gnuplot:plot (lambda ()
                                  (loop for row across history
                                        do (format t "~&~a ~a"
                                                   (nth 0 row)
                                                   (nth n row))))
                                :title name
                                :lt `(:rgb ,colour)
                                :with '(:lines)))))
