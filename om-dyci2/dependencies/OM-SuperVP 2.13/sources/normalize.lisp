;============================================================================
; OM-SuperVP
; SuperVP sound analysis and processing for OpenMusic
;============================================================================
;
;   This program is free software. For information on usage 
;   and redistribution, see the "LICENSE" file in this distribution.
;
;   This program is distributed in the hope that it will be useful,
;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
;
;============================================================================
; NORMALIZATION
; File author: J. Bresson (IRCAM - 2006-2010)
;============================================================================


(in-package :svp)
  

;; !! rescale > O
(defun svp-norm (path newpath &optional (rescale 0.0) (resolution 16))
  (let ((supervp-path (om::svp-path)))
    (if (and supervp-path (probe-file supervp-path))
        (progn
      (om::handle-new-file-exists newpath)
      (print "==========================")
      (print "SUPERVP NORMALIZATION")
      (print "==========================")
      
      (print (format nil "Input file: ~s" (namestring path)))
      (print (format nil "Level: ~D dB" rescale))
      (print (format nil "Resolution: ~D bits" resolution))
      
      (om::om-cmd-line (format nil "~s -S~S -norm ~D -Osa~D ~S" 
                                  (namestring supervp-path)
                                  (namestring path) 
                                  (abs rescale)
                                  resolution
                                  (namestring newpath)))
      )
    (om::om-beep-msg "NORMALIZE: SuperVP not found! Set path to SuperVP in the OM preferences."))
    (or (probe-file newpath)
        (om::om-beep-msg "NORMALIZE: Error in SuperVP processing. No output file created."))
    ))

(defmethod! om::supervp-normalize ((self pathname) &optional (out "normout.aif") (rescale nil) (res 16))
     :initvals '(nil "normout.aif" nil 16)
     :indoc '("sound or sound file pathname" "output file name" "scale factor" "output resolution")
     :icon 950
     :doc "Normalizes <self> in a new sound file <outfile>.

If <rescale> and <resolution> are NIL (default) they will be set according to the values in OM audio preferences.
"  
     (let ((resolution res)
           (rescaledb (or rescale 0.0))
           (outf (if out (if (pathnamep out) out (om::outfile out))
                  (om::om-choose-file-dialog :prompt "Save as..." :directory (om::outfile nil)))))
       (svp-norm self outf rescaledb resolution)))

;;; TODO: handle the case where the sound has no file (buffer)
(defmethod! om::supervp-normalize ((self om::sound) &optional (out "normout.aif") (rescale nil) (resolution nil))
   (if (om::om-sound-file-name self)
       (om::supervp-normalize (om::om-sound-file-name self) out rescale resolution)
   (om::om-beep-msg "SUPERVP-NORMALIZE: input sound file must be saved on disk!")))

;(defmethod om::general-normalize ((norm (eql :supervp)) inpath outpath val &optional resolution)
;  (supervp-normalize inpath outpath val resolution))
;(pushr :supervp *loaded-normalizers*)
;(defmethod get-def-normalize-value ((self (eql :supervp))) 0.0)
;(defmethod get-module-name ((self (eql :supervp))) "SuperVP")







