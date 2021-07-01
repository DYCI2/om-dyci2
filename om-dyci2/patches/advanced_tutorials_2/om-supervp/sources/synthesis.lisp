;============================================================================
; OM-SuperVP
; SuperVP sound analysis and processing for o7
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
; CROSS SYNTHESIS & SOURCE/FILTER SYNTHESIS
; File author: J. Bresson (IRCAM - 2006-2016)
;============================================================================

(in-package :svp)

;;;=================================================================================================
;;; SYNTHESE CROISEE SOURCE FILTRE
;;;=================================================================================================

(defmethod! om::supervp-sourcefilter-synthesis ((source t) (filtre t)  windowsize fftsize windowstep-oversamp 
                                                    window-type normalize crossmode spectenv-type outfile)
  :icon 950 
  :menuins '((4 (("1/4" 4) ("1/8" 8) ("1/16" 16) ("1/32" 32)))
             (5 (("Blackman" "blackman") ("Hanning" "hanning")("Hamming" "hamming")))
             (6 (("Normalize On" t) ("Normalize Off" nil)))
             (7 (("Amplitude+Phase" 0) ("Amplitude" 1)("Phase" 2)))
             (8 (("LPC" t) ("True Enveloppe" nil)))
             )
  :initvals '(nil nil 4096 4096 8 "hanning" nil 0 t "out.aiff")
  (let ((supervp-path (om::real-exec-pathname (om::get-pref-value :libraries :supervp-path))))
    (if (and supervp-path (probe-file supervp-path))
        (let ((srcpath (om::get-sound-file source))
              (srcpath2 (om::get-sound-file filtre)))
          (if (and (probe-file srcpath) (probe-file srcpath2))
              (let ((outpath (if (pathnamep outfile) outfile (om::outfile outfile)))
                    (command ""))
  
                (setf command (om::string+ command  
                                           (format nil "~s -t -v -Z -U -u " (namestring supervp-path))))
                
                (when normalize (setf command (om::string+ command "-norm ")))
        
                (setf command (om::string+ command  (format nil "-S~s -s~s -A " 
                                                        (namestring srcpath)
                                                        (namestring srcpath2))))
        
                (if spectenv-type (setf command (om::string+ command "-alpc ")) (setf command (om::string+ command "-atenv ")))
        
                (case crossmode
                  (0 (setf command (om::string+ command "-Gmul ")))
                  (1 (setf command (om::string+ command "-Gamul ")))
                  (2 (setf command (om::string+ command "-Gpmul "))))
        
        
                (setf command (om::string+ command  (format nil "-M~D -N~D -m~D -n~D " windowsize fftsize windowsize fftsize ) 
                                           (if windowstep-oversamp (format nil "-oversamp ~D " windowstep-oversamp) "")
                                           (format nil "-W~s  -w~s " window-type window-type)
                                           (format nil "~s " (namestring outpath)))) 
                                            
                (om::om-cmd-line command)
                (om::maybe-clean-tmp-files)
                (probe-file outpath))
            (om::om-beep-msg "Source / filter sound files no found")
            ))
      (om::om-beep-msg "SuperVP not found! Set path to SuperVP in the OM preferences."))))



;;;=================================================================================================
;;; CROSS SYNTHESIS
;;;=================================================================================================

(defmethod! om::supervp-cross-synthesis ((sound1 t) (sound2 t) (param pathname) windowsize fftsize windowstep-oversamp 
                                     window-type normalize crossmode outfile)
  :icon 950 
  :menuins '((5 (("1/4" 4) ("1/8" 8) ("1/16" 16) ("1/32" 32)))
             (6 (("Blackman" "blackman") ("Hanning" "hanning")("Hamming" "hamming")))
             (7 (("Normalize On" t) ("Normalize Off" nil)))
             (8 (("Cross" t) ("Add" nil)))
             )
  :initvals '(nil nil "" 4096 4096 8 "hanning" nil nil "out.aiff")
  (let ((supervp-path (om::real-exec-pathname (om::get-pref-value :libraries :supervp-path))))
    (if (and supervp-path (probe-file supervp-path))
        (let ((srcpath (om::get-sound-file sound1))
              (srcpath2 (om::get-sound-file sound2)))
          (if (and (probe-file srcpath) (probe-file srcpath2))
              (let ((outpath (if (pathnamep outfile) outfile (om::outfile outfile)))
                    (command ""))
                
                (setf command (om::string+ command  (format nil "~s -t -v -Z -U -u " (namestring supervp-path))))
                
                (when normalize (setf command (om::string+ command "-norm ")))

                (setf command (om::string+ command  (format nil "-S~s -s~s -A -a "  
                                                            (namestring srcpath)
                                                            (namestring srcpath2))))
        
                (if crossmode
                    (setf command (om::string+ command "-Gcross "))
                  (setf command (om::string+ command "-Gadd ")))
        
                (setf command (om::string+ command  (format nil "~s -M~D -N~D -m~D -n~D " 
                                                            (namestring param)
                                                            windowsize fftsize windowsize fftsize)
                                           (if  windowstep-oversamp (format nil "-oversamp ~D " windowstep-oversamp) "")
                                           (format nil "-W~s -w~s " window-type window-type)
                                           (format nil "~s " (namestring outpath))))
            
                (om::om-cmd-line command)
                (om::maybe-clean-tmp-files)
                (probe-file outpath))
            
            (om::om-beep-msg "Sound files no found")))
      (om::om-beep-msg "SuperVP not found! Set path to SuperVP in the OM preferences."))))


(defmethod! om::supervp-cross-synthesis ((sound1 t) (sound2 t) (param string) windowsize fftsize windowstep-oversamp 
                                     window-type normalize crossmode outfile)
  (let ((parampath (if (probe-file (pathname param)) (pathname param) (paramfile param))))
    (om::supervp-cross-synthesis sound1 sound2 parampath windowsize fftsize windowstep-oversamp 
                             window-type normalize crossmode outfile)))


(defmethod! om::supervp-cross-synthesis ((sound1 t) (sound2 t) (param om::bpf) windowsize fftsize windowstep-oversamp 
                                     window-type normalize crossmode outfile)
  (let ((parampath (make-temp-param param (paramfile "tempcross.par"))))
    (om::supervp-cross-synthesis sound1 sound2 parampath windowsize fftsize windowstep-oversamp 
                             window-type normalize crossmode outfile)))
  

(defmethod! om::supervp-cross-synthesis ((sound1 t) (sound2 t) (param list) windowsize fftsize windowstep-oversamp 
                                     window-type normalize crossmode outfile)
  (let ((parampath (make-temp-param param (paramfile "tempcross.par"))))
    (om::supervp-cross-synthesis sound1 sound2 parampath windowsize fftsize windowstep-oversamp 
                                       window-type normalize crossmode outfile)))

 
(defmethod! om::supervp-cross-synthesis ((sound1 t) (sound2 t) (param om::textbuffer) windowsize fftsize windowstep-oversamp 
                                     window-type normalize crossmode outfile)
  (let ((parampath (make-temp-param param (paramfile "tempcross.par"))))
    (om::supervp-cross-synthesis sound1 sound2 parampath windowsize fftsize windowstep-oversamp 
                                       window-type normalize crossmode outfile)))


  

