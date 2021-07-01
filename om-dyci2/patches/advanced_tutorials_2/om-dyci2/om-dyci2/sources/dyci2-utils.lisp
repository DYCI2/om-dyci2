;;;===================================================
;;;
;;; OM-DYCI2
;;;
;;;===================================================
;
;   This program is free software. For information on usage 
;   and redistribution, see the "LICENSE" file in this distribution.
;
;   This program is distributed in the hope that it will be useful,
;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
;
;============================================================================
; Authors: Jean Bresson - IRCAM/STMS 2018
;============================================================================

(in-package :dyci2)

;;;========================
;;; MIX
;;;========================

;;; duration from list of segments
;;; (- (reduce '+ (mapcar #'(lambda (seg) (- (nth 2 seg) (nth 1 seg))))) (* crossface (1- (length sequence))))
(defun concat-audiofiles (files &key (crossfade 0) (sr 44100) (out "mix.aiff") (format :wav))
   
  (unless (pathnamep out)
    (setf out (om::outfile out)))
  
  (let* ((cf-samples (om::ms->samples crossfade sr))
         (total-size (- 
                      (loop for f in files sum
                            (multiple-value-bind (format n-channels sample-rate sample-size size)
                                (audio-io::om-get-sound-info f)
                              size))
                      (* (1- (length files)) cf-samples)))
         
         (audio-mix (om::make-audio-buffer 1 total-size))
         (pos 0))
    
    (unwind-protect 
        
        (loop for f in files do 
              
              (multiple-value-bind (sbuffer format n-channels sample-rate sample-size size)
                  (audio-io::om-get-audio-buffer f)
                
                (unwind-protect 
                    
                    (dotimes (i size) 
                      (setf (fli:dereference 
                             (fli:dereference audio-mix :index 0 :type :pointer) 
                             :index (+ pos i) :type :float)
                            
                            ;;; new value
                            (if (< i cf-samples) 
                                ;;; in the crossfade segment
                                (+ (* (/ i cf-samples)  ;;; new: fade in
                                      (fli:dereference (fli:dereference sbuffer :index 0 :type :pointer) :index i :type :float))
                                   (* (- 1 (/ i cf-samples)) ;;; previous: fade out
                                      (fli:dereference (fli:dereference audio-mix :index 0 :type :pointer) :index (+ pos i) :type :float)
                                      ))
                             
                              ;;; normal
                              (fli:dereference (fli:dereference sbuffer :index 0 :type :pointer) :index i :type :float)
                              ))
                      )
                      
                  (setf pos (+ pos size (- cf-samples)))
                  (audio-io::om-free-audio-buffer sbuffer 1))
                )
              
              finally (audio-io::om-save-buffer-in-file audio-mix (namestring out) total-size 1 sr 16 format))
      
      (audio-io::om-free-audio-buffer audio-mix 1))

    out))


(compile 'concat-audiofiles)
