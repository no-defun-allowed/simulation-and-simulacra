(in-package :simulation-and-simulacra)

(defun randomize (vector)
  (dotimes (n (length vector))
    (setf (aref vector n) (random (expt 2 64)))))

(defun make-random-u64-vector (size)
  "Make a random input vector, to seed the RNGs of the workers."
  (let ((vector (make-array size :element-type '(unsigned-byte 64))))
    (randomize vector)
    vector))
(defun make-result-vector (size)
  "Make a vector to store the results of the workers in."
  (make-array (* 4 size) :element-type '(unsigned-byte 64)))

(defvar *program*)

(defun reload ()
  (setf *program*
        (eazy-opencl.host:create-program-with-source
         (third *device*)
         (alexandria:read-file-into-string
          (asdf:system-relative-pathname :simulation-and-simulacra "simulate.cl"))))
  (eazy-opencl.host:build-program *program* :devices (list (second *device*))))

(defstruct (alien-stuff (:constructor %make-alien-stuff))
  input results
  input-buffer results-buffer
  queue kernel)

(defun make-alien-stuff (jobs)
  (let ((context (third *device*)))
    (%make-alien-stuff
     :input  (make-random-u64-vector jobs)
     :input-buffer (eazy-opencl.host:create-buffer context
                                                   :mem-read-only
                                                   (* jobs 8))
     :results (make-result-vector jobs)
     :results-buffer (eazy-opencl.host:create-buffer context
                                                     :mem-read-only
                                                     (* jobs 8 4))
     :queue   (eazy-opencl.host:create-command-queue-with-properties
               context (second *device*))
     :kernel  (eazy-opencl.host:create-kernel *program* "simulate"))))

(defmacro maximize ((&rest comparisons) &body places)
  `(when (or ,@(loop for (this best) on comparisons by #'cddr
                     collect `(and ,@(loop for (this* best*) in others
                                           collect `(= ,this* ,best*))
                                   (> ,this ,best))
                     collecting (list this best) into others))
     (setf ,@places)))

(defun read-off-results (jobs results)
  (let ((best-rod-rods     0)
        (best-rod-pearls   0)
        (best-pearl-rods   0)
        (best-pearl-pearls 0))
    (declare ((unsigned-byte 64)
              best-rod-rods best-rod-pearls
              best-pearl-rods best-pearl-pearls))
    (loop for n below jobs
          for rod-rods     = (aref results      (* 4 n))
          for rod-pearls   = (aref results (+ 1 (* 4 n)))
          for pearl-rods   = (aref results (+ 2 (* 4 n)))
          for pearl-pearls = (aref results (+ 3 (* 4 n)))
          do (maximize (rod-rods   best-rod-rods
                        rod-pearls best-rod-pearls)
               best-rod-rods   rod-rods
               best-rod-pearls rod-pearls)
          do (maximize (pearl-pearls best-pearl-pearls
                        pearl-rods   best-pearl-rods)
               best-pearl-rods   pearl-rods
               best-pearl-pearls pearl-pearls))
    (values best-rod-rods best-rod-pearls
            best-pearl-rods best-pearl-pearls)))
  
(defun run-test (&key (jobs 8192)
                      (alien-stuff (make-alien-stuff jobs)))
  (let* ((inputs  (alien-stuff-input alien-stuff))
         (input-buffer (alien-stuff-input-buffer alien-stuff))
         (results (alien-stuff-results alien-stuff))
         (results-buffer (alien-stuff-results-buffer alien-stuff))
         (kernel (alien-stuff-kernel alien-stuff))
         (queue   (alien-stuff-queue alien-stuff)))
    (cffi:with-pointer-to-vector-data (inputs-ptr inputs)
      (cffi:with-pointer-to-vector-data (results-ptr results)
        (%ocl:enqueue-write-buffer queue input-buffer %ocl:true
                                   0 (* jobs 8) inputs-ptr
                                   0 (cffi:null-pointer) (cffi:null-pointer))
        (cffi:with-foreign-array (work-size '%ocl:size-t (list jobs))
          (eazy-opencl.host:set-kernel-arg kernel 0 input-buffer '%ocl:mem)
          (eazy-opencl.host:set-kernel-arg kernel 1 results-buffer '%ocl:mem)
          (%ocl:enqueue-nd-range-kernel queue kernel
                                        1 (cffi:null-pointer) work-size
                                        (cffi:null-pointer)
                                        0
                                        (cffi:null-pointer)
                                        (cffi:null-pointer)))
        (%ocl:enqueue-read-buffer queue results-buffer %ocl:true
                                  0 (* jobs 16) results-ptr
                                  0 (cffi:null-pointer) (cffi:null-pointer)))
      (randomize inputs)
      (read-off-results jobs results))))
