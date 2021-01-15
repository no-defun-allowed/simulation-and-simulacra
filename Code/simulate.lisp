(in-package :simulation-and-simulacra)

(alexandria:define-constant +binomial-table+
  (coerce
   #(#xFFFFFFFFFFFFFFFF #xFFFFCC4D8E5B5800 #xFFFD2C1CF200D000 #xFFEC2AB9775B1800
     #xFFA30678F43A4000 #xFEB7FDF86B720000 #xFC5E1ED41D0E2800 #xF75EB523A0C4D800
     #xEE4CA01ADA0E3800 #xDFF35B4202CF7000 #xCBDA8D49915DC000 #xB29ED274B93D8800
     #x95EE93BA0EF0D800 #x7826E5FC50660000 #x5BBB01DE506BD400 #x42A4D3E0B12DC000
     #x2E0F58CEF370EC00 #x1E4A3EA6852CAD00 #x12F706D5B6A42500 #x0B50A2BCD8204280
     #x067080E64CA6DBC0 #x037FE85DF2331780 #x01D17F4949B51D50 #x00E7812DA80EBE20
     #x006E548F2D2EC48C #x003271DB37642DFA #x001626C3CF96D2E8 #x00095A2944C442B3
     #x0003CCD2F63E5659 #x00017CCF6917E37B #x00008FBDC1CC6850 #x0000345D6266AF6A
     #x0000126D4532FDB5 #x00000644BF8078B9 #x0000021027D8B74B #x000000A84BD877E8
     #x00000033F51CE72F #x0000000F8D2DBB06 #x0000000484025495 #x0000000145D6265E
     #x000000005933E6CC #x0000000017BB6B20 #x0000000006239A56 #x00000000018B7DC4
     #x000000000060DD7E #x00000000001719ED #x0000000000055DF5 #x0000000000013716
     #x00000000000044AC #x0000000000000EC9 #x000000000000031B #x00000000000000A3
     #x0000000000000020 #x0000000000000006 #x0000000000000001 #x0000000000000000)
   '(vector (unsigned-byte 64)))
  :test #'equalp)
(defconstant +binomial-table-length+ 56)

(declaim (inline pearl-drops next-random))
(defun pearl-drops (uniform64)
  (declare ((unsigned-byte 64) uniform64)
           (optimize (speed 3)))
  (let ((first 0)
        (last  (1- +binomial-table-length+)))
    (loop until (= first last)
          do (let ((middle (ceiling (+ first last) 2)))
               (if (>= uniform64 (aref +binomial-table+ middle))
                   (setf last  (1- middle))
                   (setf first middle))))
    (the (unsigned-byte 8) first)))

(declaim (ftype (function ((unsigned-byte 64))
                          (values (unsigned-byte 32) (unsigned-byte 32)))
                next-random))
(defun next-random (state)
  (declare ((unsigned-byte 64) state)
           (optimize (speed 3)))
  (let ((c (ldb (byte 32 32) state))
        (x (ldb (byte 32 0)  state)))
    (values (ldb (byte 64 0) (+ c (* x 4294883355)))
            (logxor c x))))

(defun simulate ()
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (debug 0)
                     (compilation-speed 0)))
  (let ((seed (random (expt 2 64)))
        (best-rod-rods 0)
        (best-rod-pearls 0)
        (best-pearl-rods 0)
        (best-pearl-pearls 0))
    (declare ((unsigned-byte 64) seed)
             ((unsigned-byte 16)
              best-rod-rods best-rod-pearls
              best-pearl-rods best-pearl-pearls))
    (flet ((sample ()
             (multiple-value-bind (next-seed value)
                 (next-random seed)
               (setf seed next-seed)
               value)))
      (declare (inline sample)
               (dynamic-extent #'sample))
      (dotimes (n +kernel-iterations+)
        (let ((pearls 0)
              (rods 0))
          (declare ((unsigned-byte 16) pearls rods))
          ;; Sample the rods using LOGCOUNT.
          (dotimes (n (floor 305 32))
            (incf rods (logcount (sample))))
          (incf rods (logcount (ldb (byte (mod 305 32) 0) (sample))))
          ;; Sample the pearls, using our table.
          (setf pearls (pearl-drops (logior (sample)
                                            (ash (sample) 32))))
          ;; Update best results.
          (multiple-value-setq (best-rod-rods best-rod-pearls)
            (maximize-rods rods pearls best-rod-rods best-rod-pearls))
          (multiple-value-setq (best-pearl-rods best-pearl-pearls)
            (maximize-pearls rods pearls best-pearl-rods best-pearl-pearls)))))
    (vector best-rod-rods best-rod-pearls best-pearl-rods best-pearl-pearls)))
