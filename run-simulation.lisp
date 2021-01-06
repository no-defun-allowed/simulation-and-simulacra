(in-package :simulation-and-simulacra)

(defun randomize (vector)
  (dotimes (n (length vector))
    (setf (aref vector n) (random (expt 2 48)))))

(defun make-random-u64-vector (size)
  "Make a random input vector, to seed the RNGs of the workers."
  (let ((vector (make-array size :element-type '(unsigned-byte 64))))
    (randomize vector)
    vector))
(defun make-result-vector (size)
  "Make a vector to store the results of the workers in."
  (make-array (* 2 size) :element-type '(unsigned-byte 64)))

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
                                                     (* jobs 16))
     :queue   (eazy-opencl.host:create-command-queue-with-properties
               context (second *device*))
     :kernel  (eazy-opencl.host:create-kernel *program* "simulate"))))

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
                                            (cffi:null-pointer))
              (eazy-opencl.bindings:finalize-box kernel))
        ;; The thing about using finalizers for foreign memory is that the
        ;; host is rarely going to find a need to garbage collect when you
        ;; want it to.
        (eazy-opencl.bindings:finalize-box input-buffer)
        (eazy-opencl.bindings:finalize-box results-buffer)
        (%ocl:enqueue-read-buffer queue results-buffer %ocl:true
                                  0 (* jobs 16) results-ptr
                                  0 (cffi:null-pointer) (cffi:null-pointer)))
      (randomize inputs)
      (loop for n below jobs
            maximizing (aref results (* 2 n))      into maximum-rods
            maximizing (aref results (1+ (* 2 n))) into maximum-pearls
            finally (return (values maximum-rods maximum-pearls))))))
