;;;; linker.lisp — Linker for secd-lisp
;;;;
;;;; Copyright (C) 2026
;;;; License: GPL3
;;;;
;;;; Generates UF2 from firmware .bin + compiled bytecode.
;;;; Creates fresh UF2 from scratch — no merging/patching needed.

(in-package #:secd-lisp)

;;; UF2 constants
(defconstant +uf2-magic-start0+ #x0A324655)
(defconstant +uf2-magic-start1+ #x9E5D5157)
(defconstant +uf2-magic-end+    #x0AB16F30)
(defconstant +uf2-flag-family+  #x2000)
(defconstant +uf2-flag-not-main-flash+ #x8000)
(defconstant +rp2040-family-id+         #xE48BFF56)
(defconstant +rp2350-arm-s-family-id+   #xE48BFF59)
(defconstant +rp2350-riscv-family-id+   #xE48BFF5A)
(defconstant +rp2350-arm-ns-family-id+  #xE48BFF5B)
(defconstant +samd21-family-id+         #x68ED2B88)
(defconstant +rp2040-flash-base+ #x10000000)
(defconstant +samd21-flash-base+ #x00002000)
(defconstant +rp2040-page-size+ 256)
(defconstant +uf2-payload-size+ 256)

(defun uf2-family-id (family-name)
  "Map a metadata output.family string to a UF2 family id.
Defaults to RP2040 for nil/unknown families."
  (cond
    ((string-equal family-name "rp2040")       +rp2040-family-id+)
    ((string-equal family-name "rp2350-arm-s") +rp2350-arm-s-family-id+)
    ((string-equal family-name "rp2350-riscv") +rp2350-riscv-family-id+)
    ((string-equal family-name "rp2350-arm-ns") +rp2350-arm-ns-family-id+)
    ((string-equal family-name "samd21")       +samd21-family-id+)
    (t (when family-name
         (warn "Unknown UF2 family ~S, defaulting to RP2040" family-name))
       +rp2040-family-id+)))

(defun uf2-flash-base (family-name)
  "Flash base address for a metadata output.family string (absolute UF2 target base)."
  (cond
    ((string-equal family-name "samd21") +samd21-flash-base+)
    (t +rp2040-flash-base+)))

;;; ESP32 (esp32s3): firmware.bin is concatenated with the SECD-header bytecode
;;; (a plain `cat firmware.bin bytecode.secd > final.bin`).  No fixed bytecode
;;; offset: the firmware locates the bytecode at runtime by scanning past its
;;; own image end, so firmware size changes (added/removed features) never
;;; require re-linking or re-flashing a different layout.

(defun machine-family-name (machine-path)
  "Read output.family from the .machine metadata.json (or nil)."
  (zip:with-zipfile (zip machine-path)
    (let ((entry (zip:get-zipfile-entry "metadata.json" zip)))
      (when entry
        (let* ((metadata-data (zip:zipfile-entry-contents entry))
               (metadata (yason:parse
                          (map 'string #'code-char metadata-data)))
               (output (gethash "output" metadata)))
          (when output
            (gethash "family" output)))))))

;;; Helper: write 32-bit LE
(defun write-u32-le (buf offset value)
  (setf (aref buf offset) (logand value #xFF))
  (setf (aref buf (+ offset 1)) (logand (ash value -8) #xFF))
  (setf (aref buf (+ offset 2)) (logand (ash value -16) #xFF))
  (setf (aref buf (+ offset 3)) (logand (ash value -24) #xFF)))

;;; Helper: read 32-bit LE
(defun read-u32-le (buf offset)
  (+ (aref buf offset)
     (ash (aref buf (+ offset 1)) 8)
     (ash (aref buf (+ offset 2)) 16)
     (ash (aref buf (+ offset 3)) 24)))

;;; Write a single UF2 block. Data is padded to 256 bytes so payload_size is
;;; always 256, matching pico-sdk elf2uf2 (RP2040 bootrom expects this).
(defun write-uf2-block (stream addr data block-no total-blocks family-id)
  "Write a single UF2 block."
  (let* ((buf (make-array 512 :element-type '(unsigned-byte 8) :initial-element 0))
         (padded (make-array +uf2-payload-size+
                             :element-type '(unsigned-byte 8)
                             :initial-element 0)))
    (replace padded data)
    ;; Magic
    (write-u32-le buf 0 +uf2-magic-start0+)
    (write-u32-le buf 4 +uf2-magic-start1+)
    ;; Flags (bit 13 = family ID present)
    (write-u32-le buf 8 +uf2-flag-family+)
    ;; Target address
    (write-u32-le buf 12 addr)
    ;; Payload size (always 256)
    (write-u32-le buf 16 +uf2-payload-size+)
    ;; Block number
    (write-u32-le buf 20 block-no)
    ;; Total blocks
    (write-u32-le buf 24 total-blocks)
    ;; Family ID
    (write-u32-le buf 28 family-id)
    ;; Data
    (replace buf padded :start1 32)
    ;; Final magic
    (write-u32-le buf 508 +uf2-magic-end+)
    (write-sequence buf stream)))

;;; Generate UF2 blocks from a raw binary blob at a given flash address
(defun bin-to-uf2-blocks (bin-data base-addr)
  "Convert raw binary data to a list of (addr . data) UF2 payload chunks."
  (let ((chunks nil)
        (offset 0))
    (loop while (< offset (length bin-data))
          do (let ((chunk (subseq bin-data offset
                                   (min (+ offset +uf2-payload-size+)
                                        (length bin-data)))))
               (push (cons (+ base-addr offset) chunk) chunks)
               (incf offset +uf2-payload-size+)))
    (nreverse chunks)))

;;; Convert a UF2 file back to raw flash binary
(defun uf2-to-bin (uf2-data base-addr)
  "Extract raw flash content from UF2 blocks as a byte array.
Blocks flagged 'not main flash' (e.g. the RP2350 flash-top sentinel from
elf2uf2) are skipped; they are not application code."
  (let ((max-end 0))
    (loop for off from 0 below (length uf2-data) by 512
          do (when (and (= (read-u32-le uf2-data off) +uf2-magic-start0+)
                        (zerop (logand (read-u32-le uf2-data (+ off 8))
                                       +uf2-flag-not-main-flash+)))
               (let* ((addr (read-u32-le uf2-data (+ off 12)))
                      (payload (read-u32-le uf2-data (+ off 16)))
                      (end (- (+ addr payload) base-addr)))
                 (setf max-end (max max-end end)))))
    (let ((bin (make-array max-end :element-type '(unsigned-byte 8)
                                     :initial-element #xFF)))
      (loop for off from 0 below (length uf2-data) by 512
            do (when (and (= (read-u32-le uf2-data off) +uf2-magic-start0+)
                          (zerop (logand (read-u32-le uf2-data (+ off 8))
                                         +uf2-flag-not-main-flash+)))
                 (let* ((addr (read-u32-le uf2-data (+ off 12)))
                        (payload (read-u32-le uf2-data (+ off 16)))
                        (dst (- addr base-addr)))
                   (replace bin uf2-data :start1 dst :end1 (+ dst payload)
                                           :start2 (+ off 32)
                                           :end2 (+ off 32 payload)))))
      bin)))

;;; Generate fresh UF2 from firmware binary + bytecode
(defun generate-uf2 (firmware-bin bytecode output-file
                      &optional (family-id +rp2040-family-id+)
                                (flash-base +rp2040-flash-base+))
  "Generate a brand new UF2 file from firmware .bin and bytecode.
Bytecode is placed contiguously after firmware at next page-aligned address."
  (let* ((firmware-size (length firmware-bin))
         (bytecode-addr (+ flash-base
                           (* (ceiling firmware-size +rp2040-page-size+)
                              +rp2040-page-size+)))
         (firmware-chunks (bin-to-uf2-blocks firmware-bin flash-base))
         (bytecode-chunks (bin-to-uf2-blocks bytecode bytecode-addr))
         (total-blocks (+ (length firmware-chunks) (length bytecode-chunks)))
         (block-no 0))
    
    (with-open-file (stream output-file :direction :output
                                         :element-type '(unsigned-byte 8)
                                         :if-exists :supersede)
      ;; Write firmware blocks
      (dolist (chunk firmware-chunks)
        (write-uf2-block stream (car chunk) (cdr chunk) block-no total-blocks
                         family-id)
        (incf block-no))
      
      ;; Write bytecode blocks
      (dolist (chunk bytecode-chunks)
        (write-uf2-block stream (car chunk) (cdr chunk) block-no total-blocks
                         family-id)
        (incf block-no)))
    
    (format t "Firmware: ~A bytes, bytecode at 0x~8,'0X~%"
            firmware-size bytecode-addr)
    (format t "Generated UF2: ~A (~A blocks, ~A bytes)~%"
            output-file total-blocks (* total-blocks 512))
    output-file))

;;; Bytecode version
(defvar *bytecode-version-major* 0)
(defvar *bytecode-version-minor* 1)

;;; Serialize bytecode with header
(defun serialize-bytecode (bytecode)
  "Serialize bytecode with SECD header."
  (let* ((header-size 14)
         (total-size (+ header-size (length bytecode)))
         (result (make-array total-size :element-type '(unsigned-byte 8))))
    ;; Magic "SECD"
    (setf (aref result 0) (char-code #\S))
    (setf (aref result 1) (char-code #\E))
    (setf (aref result 2) (char-code #\C))
    (setf (aref result 3) (char-code #\D))
    ;; Version major/minor (bytes 4-5)
    (setf (aref result 4) *bytecode-version-major*)
    (setf (aref result 5) *bytecode-version-minor*)
    ;; Reserved (bytes 6-7)
    (setf (aref result 6) 0)
    (setf (aref result 7) 0)
    ;; Code size (16-bit big-endian)
    (setf (aref result 8) (logand (ash (length bytecode) -8) #xFF))
    (setf (aref result 9) (logand (length bytecode) #xFF))
    ;; Constants size 0
    (setf (aref result 10) 0)
    (setf (aref result 11) 0)
    ;; Symbols size 0
    (setf (aref result 12) 0)
    (setf (aref result 13) 0)
    ;; Copy bytecode
    (replace result bytecode :start1 header-size)
    result))

;;; Link bytecode with firmware binary to produce UF2
(defun link-with-firmware (bytecode firmware-bin-path output-path
                           &optional (family-id +rp2040-family-id+)
                                     (flash-base +rp2040-flash-base+))
  "Link compiled bytecode with firmware .bin to produce UF2.
Bytecode is appended to firmware binary at next page-aligned address."
  (let* ((firmware-bin (with-open-file (s firmware-bin-path
                                          :element-type '(unsigned-byte 8))
                         (let ((buf (make-array (file-length s)
                                                :element-type '(unsigned-byte 8))))
                           (read-sequence buf s)
                           buf)))
         (bytecode-with-header (serialize-bytecode bytecode))
         ;; Pad firmware to next page boundary
         (padded-size (* (ceiling (length firmware-bin) +rp2040-page-size+)
                         +rp2040-page-size+))
         (padded-firmware (make-array padded-size :element-type '(unsigned-byte 8)
                                                 :initial-element #xFF))
         ;; Concatenate: padded firmware + bytecode
         (combined (make-array (+ padded-size (length bytecode-with-header))
                               :element-type '(unsigned-byte 8))))
    (replace combined firmware-bin)
    (replace combined bytecode-with-header :start1 padded-size)
    (format t "Firmware: ~A bytes (padded to ~A)~%" (length firmware-bin) padded-size)
    (format t "Bytecode: ~A bytes~%" (length bytecode))
    (generate-uf2 combined #() output-path family-id flash-base)))

;;; Link command: takes .machine file + compiled bytecode, produces .uf2
(defun machine-uf2-config (machine-path)
  "Read output.family from the .machine metadata.json.
Returns (values family-id flash-base)."
  (zip:with-zipfile (zip machine-path)
    (let ((entry (zip:get-zipfile-entry "metadata.json" zip)))
      (when entry
        (let* ((metadata-data (zip:zipfile-entry-contents entry))
               (metadata (yason:parse
                          (map 'string #'code-char metadata-data)))
               (output (gethash "output" metadata)))
          (when output
            (let ((family-name (gethash "family" output)))
              (return-from machine-uf2-config
                (values (uf2-family-id family-name)
                        (uf2-flash-base family-name)))))))))
  (values +rp2040-family-id+ +rp2040-flash-base+))

(defun read-zip-entry (zip-path entry-name)
  "Read a raw byte array entry out of a .machine zip."
  (zip:with-zipfile (zip zip-path)
    (let ((entry (zip:get-zipfile-entry entry-name zip)))
      (unless entry (error "No ~A in ~A" entry-name zip-path))
      (zip:zipfile-entry-contents entry))))

(defun write-raw-file (path bytes)
  "Write BYTES to PATH, superseding anything already there."
  (with-open-file (stream path :direction :output
                                 :element-type '(unsigned-byte 8)
                                 :if-exists :supersede)
    (write-sequence bytes stream)))

(defun bytecode-path (output-path)
  "Path of the standalone .secd bytecode file next to OUTPUT-PATH."
  (merge-pathnames (make-pathname :name (pathname-name output-path)
                                  :type "secd")
                   output-path))

(defun link-machine-esp32 (machine-path bytecode output-path)
  "ESP32 link: concatenate firmware.bin with the SECD-header bytecode and write
the standalone .secd alongside. Flash the .bin at the app offset (0x10000); the
bytecode follows the firmware image, located at runtime by scanning.
Equivalent to:  cat firmware.bin <bytecode>.secd > final.bin"
  (let* ((firmware (read-zip-entry machine-path "firmware.bin"))
         (bytecode-with-header (serialize-bytecode bytecode))
         (combined (make-array (+ (length firmware)
                                  (length bytecode-with-header))
                               :element-type '(unsigned-byte 8))))
    (replace combined firmware)
    (replace combined bytecode-with-header :start1 (length firmware))
    (write-raw-file output-path combined)
    (write-raw-file (bytecode-path output-path) bytecode-with-header)
    (format t "Firmware: ~A bytes~%" (length firmware))
    (format t "Bytecode: ~A bytes (header + ~A)~%"
            (length bytecode-with-header) (length bytecode))
    (format t "Final image: ~A~%" output-path))
  output-path)

(defun link-machine (machine-path bytecode output-path)
  "Link bytecode with a .machine file to produce the final flash image.
UF2 targets (rp2040/rp2350/samd21) get a .uf2; ESP32 and bare-metal STM32
targets get a single concatenated .bin (see link-machine-esp32)."
  (let ((family-name (machine-family-name machine-path)))
    (if (and family-name
             (or (string-equal family-name "esp32s2")
                 (string-equal family-name "esp32s3")
                 (string-equal family-name "esp32c3")
                 (string-equal family-name "stm32f103")
                 (string-equal family-name "stm32f401")))
        (link-machine-esp32 machine-path bytecode output-path)
        (link-machine-uf2 machine-path bytecode output-path))))

(defun link-machine-uf2 (machine-path bytecode output-path)
  "Link bytecode with .machine file to produce .uf2.
The .machine file contains firmware.uf2 + metadata.json. The firmware
blocks are extracted back to raw binary, then re-linked with the bytecode."
  (multiple-value-bind (family-id flash-base)
      (machine-uf2-config machine-path)
    (let* ((temp-dir (merge-pathnames #p".secd-lisp/tmp/" (user-homedir-pathname)))
           (firmware-bin-path (merge-pathnames "firmware.bin" temp-dir)))
      (ensure-directories-exist temp-dir)
      ;; Extract firmware.uf2 from .machine and convert to raw binary
      (zip:with-zipfile (zip machine-path)
        (let ((entry (zip:get-zipfile-entry "firmware.uf2" zip)))
          (unless entry (error "No firmware.uf2 in ~A" machine-path))
          (let* ((uf2-data (zip:zipfile-entry-contents entry))
                 (firmware-bin (uf2-to-bin uf2-data flash-base)))
            (with-open-file (stream firmware-bin-path :direction :output
                                                      :element-type '(unsigned-byte 8)
                                                      :if-exists :supersede)
              (write-sequence firmware-bin stream)))))
      ;; Link raw firmware with bytecode
      (link-with-firmware bytecode firmware-bin-path output-path family-id
                          flash-base))))
