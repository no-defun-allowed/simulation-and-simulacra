(in-package :simulation-and-simulacra)

(defun make-random-u64-vector (size)
  (let ((vector (make-array size :element-type '(unsigned-byte 64))))
    (dotimes (n size)
      (setf (aref vector n) (random (expt 2 48))))
    vector))
(defun make-result-vector (size)
  (make-array (* 2 size) :element-type '(unsigned-byte 64)))

(defvar *program*)

(defun reload ()
  (setf *program*
        (eazy-opencl.host:create-program-with-source
         (third *device*)
         (alexandria:read-file-into-string "simulate.cl")))
  (eazy-opencl.host:build-program *program* :devices (list (second *device*))))

(defun run-test (&key (jobs 8192))
  (let* ((inputs  (make-random-u64-vector jobs))
         (results (make-result-vector jobs))
         (context (third *device*))
         (queue   (eazy-opencl.host:create-command-queue-with-properties
                   context (second *device*))))
    (cffi:with-pointer-to-vector-data (inputs-ptr inputs)
      (cffi:with-pointer-to-vector-data (results-ptr results)
        (let ((input-buffer (eazy-opencl.host:create-buffer context
                                                            :mem-read-only
                                                            (* jobs 8)))
              (results-buffer (eazy-opencl.host:create-buffer context
                                                              :mem-read-only
                                                              (* jobs 16))))
          (%ocl:enqueue-write-buffer queue input-buffer %ocl:true
                                     0 (* jobs 8) inputs-ptr
                                     0 (cffi:null-pointer) (cffi:null-pointer))
          (cffi:with-foreign-array (work-size '%ocl:size-t (list jobs))
            (let ((kernel (eazy-opencl.host:create-kernel *program* "simulate")))
              (eazy-opencl.host:set-kernel-arg kernel 0 input-buffer '%ocl:mem)
              (eazy-opencl.host:set-kernel-arg kernel 1 results-buffer '%ocl:mem)
              (%ocl:enqueue-nd-range-kernel queue kernel
                                            1 (cffi:null-pointer) work-size
                                            (cffi:null-pointer)
                                            0
                                            (cffi:null-pointer)
                                            (cffi:null-pointer))))
          (%ocl:enqueue-read-buffer queue results-buffer %ocl:true
                                    0 (* jobs 16) results-ptr
                                    0 (cffi:null-pointer) (cffi:null-pointer)))))
    (loop for n below jobs
          maximizing (aref results (* 2 n))      into maximum-rods
          maximizing (aref results (1+ (* 2 n))) into maximum-pearls
          finally (return (values maximum-rods maximum-pearls)))))
          
