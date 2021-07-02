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
;;; SOUND PROCESSING
;;; Transposition, Pitch shifting, Time stretching, Band filter, Format filter, 
;;; Surface filter, Break-point filter, Clipping, Freezing, Gain ...
;;;
;;; Authors: Jean Bresson & Jean Lochard (IRCAM - 2006-2016)
;===================================================

(in-package :svp)
  
;;;================ 
;;; PARAM FILES 
;;;================

(defvar *param-files-folder* nil)

(defmethod set-paramfiles-folder ((path string))
  (om::set-pref :libraries :supervp-temp-folder (pathname path)))

(defmethod set-paramfiles-folder ((path pathname))
  (om::set-pref :libraries :supervp-temp-folder (pathname path)))

(defmethod paramfile ((name string))
  (merge-pathnames name (om::get-pref-value :libraries :supervp-temp-folder)))

(defmethod paramfile ((name null)) *param-files-folder*)

;;; GEREATE A PARAM FILE

(defmethod make-temp-param ((self om::bpf) &optional (filename "svp-tmp-param.par"))
  (namestring (om::add-tmp-file (om::save-as-text self (if (pathnamep filename) filename (paramfile filename))))))   

(defmethod make-temp-param ((self om::textbuffer) &optional (filename "svp-tmp-param.par"))
  (namestring (om::add-tmp-file (om::save-as-text self (if (pathnamep filename) filename (paramfile filename))))))

(defmethod make-temp-param ((self list) &optional (filename "svp-tmp-param.par"))
  (namestring (om::add-tmp-file (om::save-as-text self (if (pathnamep filename) filename (paramfile filename))))))

(defmethod make-temp-param ((self pathname) &optional filename)
  (declare (ignore filename))
  (namestring self))

(defmethod make-temp-param ((self string) &optional filename)
  (declare (ignore filename))
  (namestring (if (probe-file (pathname self)) self (paramfile self))))

(defmethod make-temp-param ((self number) &optional (filename "svp-tmp-param.par"))
  (format nil "~f" self))

(defmethod make-temp-param ((self integer) &optional (filename "svp-tmp-param.par"))
  (format nil "~D" self))

;;;================ 
;;; FORMAT SVP COMMAND
;;;================ 

(defun make-svp-command (srcpath processings begin end
                                 windowsize fftsize windowstep-oversamp
                                 window-type shape-invariant preserve-transient
                                 normalize audio-res
                                 outpath)
  (let ((supervp-path (om::real-exec-pathname (om::get-pref-value :libraries :supervp-path))))
    (if (and supervp-path (probe-file supervp-path))
        (let ((srcpath (pathname srcpath))
              (beginstr (if begin (format nil "-B~f " begin) "")) 
              (endstr (if end (format nil "-E~f " end) "")) 
              (command ""))
          (when (probe-file srcpath)
             
            (setf command (om::string+ command (format nil "~s -t -Z " (namestring supervp-path))))
             
            (when normalize (setf command (om::string+ command "-norm ")))
        
            (setf command (om::string+ command (format nil "-U -S~s -Afft " (namestring srcpath))))

            (setf command (om::string+ command beginstr endstr))
             
            (setf command (om::string+ command (format nil "-M~D -N~D -W~D " windowsize fftsize  window-type)
                                       (if windowstep-oversamp (format nil "-oversamp ~D " windowstep-oversamp ""))))

            (when shape-invariant (setf command (om::string+ command "-shape 2 -Vuf -4 ")))
        
            ;;; transients
            (if preserve-transient
                (let ((tr-settings (when preserve-transient
                                     (if (listp preserve-transient) preserve-transient
                                       '(1.4 (0.0 22050.0) 1.5 nil)))))

                  (setf command (om::string+ command (format nil "-P1 -td_thresh ~f -td_band ~f,~f -td_ampfac ~f "
                                                             (nth 0 tr-settings)
                                                             (car (nth 1 tr-settings))
                                                             (cadr (nth 1 tr-settings))
                                                             (nth 2 tr-settings)
                                                             )))
                  (when (nth 4 tr-settings)
                    (setf command (om::string+ command (format nil "-td_relaxto ~f -td_relax ~f ")
                                               (car (nth 3 tr-settings))
                                               (cadr (nth 3 tr-settings))
                                               )))
                  )
              (unless (find "-Ffshift" (om::list! processings) :test #'(lambda (s1 s2) 
                                                                         (and (>= (length s2) 8)  
                                                                              (string-equal s1 s2 :end1 8 :end2 8))))
                ;;; option -P0 (phase synchronization) incompatible avec freq shifting
                (setf command (om::string+ command (format nil "-P0 "))))
              )
        
            ;;; processings
            (loop for p in (om::list! processings) do (setf command (om::string+ command p " ")))
        
            ;;; out
            (setf command (om::string+ command (format nil "-Osa~D ~s" audio-res (namestring outpath))))
        
            command))
  
      (om::om-beep-msg "SuperVP not found! Set path to SuperVP in the OM preferences.")
      )))



(defmethod! om::supervp-processing ((src pathname) processings begin end windowsize fftsize windowstep-oversamp 
                                        window-type shape-invariant preserve-transient
                                        normalize outfile)
  :icon 950 
  :menuins '((6 (("1/4" 4) ("1/8" 8) ("1/16" 16) ("1/32" 32)))
             (7 (("Blackman" "blackman") ("Hanning" "hanning")("Hamming" "hamming")))
             (8 (("Shape Invariant On" t) ("Shape Invariant Off" nil)))
             (10 (("Normalize On" t) ("Normalize Off" nil))))
  :initvals '(nil "" nil nil 4096 4096 8 "hanning" nil t nil "out.aiff")
  (let* ((outpath (om::handle-new-file-exists (if (pathnamep outfile) outfile (om::outfile outfile))))
         (cmd (make-svp-command src processings begin end windowsize fftsize windowstep-oversamp 
                                window-type shape-invariant preserve-transient normalize 16
                                outpath)))
    (when cmd
      (print "==========================")
      (print "SUPERVP PROCESSING")
      (print "==========================")
      
      (print (format nil "Input file: ~s" (namestring src)))
      (print (format nil "Output file: ~s" (namestring outpath)))
     
      (om::om-cmd-line cmd)
      (om::maybe-clean-tmp-files)
      (probe-file outpath))))
    
(defmethod! om::supervp-processing ((src string) processings begin end windowsize fftsize windowstep-oversamp 
                                    window-type shape-invariant preserve-transient
                                    normalize outfile)
  (let ((file (if (probe-file (pathname src)) (pathname src) (om::infile src))))
    (om::supervp-processing file 
                        processings begin end windowsize fftsize windowstep-oversamp 
                        window-type shape-invariant preserve-transient normalize
                        outfile)))
 
(defmethod! om::supervp-processing ((src om::sound) processings begin end windowsize fftsize windowstep-oversamp 
                                     window-type shape-invariant preserve-transient
                                     normalize outfile)
     (if (om::file-pathname src)
         (om::supervp-processing (om::file-pathname src) 
                             processings begin end windowsize fftsize windowstep-oversamp 
                             window-type shape-invariant preserve-transient normalize
                             outfile)
         (om::om-beep-msg "SUPERVP-PROCESSING: input sound file must be saved on disk!")))


;;;=================================================================================================
;;; TIME STRETCH
;;;=================================================================================================

(defmethod! om::supervp-timestretch ((self number))
    :icon 951 
    (format nil "-D~f" self))

(defmethod! om::supervp-timestretch ((self pathname))
    (format nil "-D~s" (namestring self)))

(defmethod! om::supervp-timestretch ((self string))
    (format nil "-D~s" (namestring (if (probe-file (pathname self)) self (paramfile self)))))

(defmethod! om::supervp-timestretch ((self om::textbuffer))
  (let ((tmpfile (make-temp-param self (paramfile "temptimestretch.par"))))
    (format nil "-D\"~a\"" (namestring tmpfile))))

(defmethod! om::supervp-timestretch ((self om::bpf))
  (let ((tmpfile (make-temp-param self (paramfile "temptimestretch.par"))))
    (format nil "-D\"~a\"" (namestring tmpfile))))

(defmethod! om::supervp-timestretch ((self list))
  (let ((tmpfile (make-temp-param self (paramfile "temptimestretch.par"))))
    (format nil "-D\"~a\"" (namestring tmpfile))))


;;;=================================================================================================
;;; TRANSPOSITION
;;;=================================================================================================

;;; GET TRANS PARAMS 
;;; returns number or list ((t1 val1) (t2 val2) ...)
(defmethod get-trans-params ((self number)) self)
(defmethod get-trans-params ((self list)) self)
(defmethod get-trans-params ((self om::bpf)) (om::point-pairs self))
(defmethod get-trans-params ((self om::textbuffer)) (om:textbuffer-read self :lines-cols))
(defmethod get-trans-params ((self string)) (get-trans-params (pathname self)))
(defmethod get-trans-params ((self pathname))
  (om::format-from-text-lines 
   (remove "" (om::lines-from-file self) :test 'string-equal)
   :lines-cols))
 

;;; returns 2 lists sampled at the same times...
(defun interpole-envelopes (env1 env2)
  (let (newenv1pts newenv2pts)
    (loop for element in env1 do
          (unless (member (car element) env2 :test '= :key 'car)
            (push (list (car element) (om::x-transfer env2 (car element))) newenv2pts)
            ))
    (loop for element in env2 do
          (unless (member (car element) env1 :test '= :key 'car)
            (push (list (car element) (om::x-transfer env1 (car element))) newenv1pts)
            ))
    (values 
     (sort (append env1 newenv1pts) '< :key 'car)
     (sort (append env2 newenv2pts) '< :key 'car)
   )))

;;; transposition + envelope
(defmethod trans-params-envelope ((trans number) (env number) trstr)
  (format nil "-transke ~D ~A ~D" (- trans env) trstr env))

(defmethod trans-params-envelope ((trans list) (env number) trstr)
  (let ((trlist (mapcar #'(lambda (elem) (list (car elem) (- (cadr elem) env))) trans)))
    (format nil "-transke \"~a\" ~A ~D" (make-temp-param trlist "temp-transp.par") trstr env)))
    
(defmethod trans-params-envelope ((trans number) (env list) trstr)
  (let ((trlist (mapcar #'(lambda (elem) (list (car elem) (- trans (cadr elem)))) env)))
    (format nil "-transke \"~a\" ~A \"~a\"" (make-temp-param trlist "temp-transp.par") trstr (make-temp-param env "temp-env-transp.par"))
    ))

(defmethod trans-params-envelope ((trans list) (env list) trstr)
    (multiple-value-bind (itrans ienv)
        (interpole-envelopes trans env)
      (let ((trlist (mapcar #'(lambda (tr en) (list (car tr) (- (cadr tr) (cadr en)))) itrans ienv)))
        (format nil "-transke \"~a\" ~A \"~a\"" (make-temp-param trlist "temp-transp.par") trstr (make-temp-param env "temp-env-transp.par")))))

;;; transposition seule
(defun trans-params-no-envelope (param trstr)
  (om::string+ trstr " " (make-temp-param param "temptransp.par")))

;;; full command line
(defun transposition-cmd (param preserve-enveloppe env order envtrans timecor mode)
  (let ((trans-string (if timecor "-trans" "-transnc")))
    (if preserve-enveloppe
        (om::string+ (trans-params-envelope (get-trans-params param) (get-trans-params envtrans) trans-string) 
                   " -Afft " (om::number-to-string order) (if env "t" "")
                   " -transmo " (om::number-to-string mode))
      (om::string+ " " (trans-params-no-envelope param trans-string) 
                     " -Afft" 
                     " -transmo " (om::number-to-string mode))
      )))
 
;;; OM Methods 
(defmethod! om::supervp-transposition ((self number) preserve-enveloppe enveloppe-type filter-order &optional (envtransp 0) (time-correction t) (mode 0))
    :icon 951 
    :menuins '((1 (("Preserve Enveloppe On" t) ("Preserve Enveloppe Off" nil)))
               (2 (("True Enveloppe" t) ("LPC Enveloppe" nil)))
               (5 (("Time correction On" t) ("Time correction Off" nil)))
               (6 (("Time Domain" 0) ("Automatic" 1) ("Frequency Domain" 2))))
    :initvals '(0 t t 70 0 t 0)
   (transposition-cmd self preserve-enveloppe enveloppe-type filter-order envtransp time-correction mode))

(defmethod! om::supervp-transposition ((self pathname) preserve-enveloppe enveloppe-type filter-order &optional (envtransp 0) (time-correction t) (mode 0))
    (transposition-cmd self preserve-enveloppe enveloppe-type filter-order envtransp time-correction mode))

(defmethod! om::supervp-transposition ((self string) preserve-enveloppe enveloppe-type filter-order &optional (envtransp 0) (time-correction t) (mode 0))
    (transposition-cmd self preserve-enveloppe enveloppe-type filter-order envtransp time-correction mode))

(defmethod! om::supervp-transposition ((self om::textbuffer) preserve-enveloppe enveloppe-type filter-order &optional (envtransp 0) (time-correction t) (mode 0))
      (transposition-cmd self preserve-enveloppe enveloppe-type filter-order envtransp time-correction mode))

(defmethod! om::supervp-transposition ((self om::bpf) preserve-enveloppe enveloppe-type filter-order &optional (envtransp 0) (time-correction t) (mode 0))
      (transposition-cmd self preserve-enveloppe enveloppe-type filter-order envtransp time-correction mode))

(defmethod! om::supervp-transposition ((self list) preserve-enveloppe enveloppe-type filter-order &optional (envtransp 0) (time-correction t) (mode 0))
    (transposition-cmd self preserve-enveloppe enveloppe-type filter-order envtransp time-correction mode))


;;;=================================================================================================
;;; FREQUENCY-SHIFTING
;;;=================================================================================================

(defmethod! om::supervp-frequencyshift ((self number))
    :icon 951 
    (let ((tmpfile (make-temp-param (list (list 0 self)) (paramfile "fshift.par"))))
      (format nil "-Ffshift \"~a\"" tmpfile)))

(defmethod! om::supervp-frequencyshift ((self pathname))
    (format nil "-Ffshift \"~a\"" (namestring self)))

(defmethod! om::supervp-frequencyshift ((self string))
    (format 
     nil "-Ffshift \"~a\"" 
     (namestring (if (probe-file (pathname self)) self (paramfile self)))))

(defmethod! om::supervp-frequencyshift ((self om::textbuffer))
    (let ((tmpfile (make-temp-param self (paramfile "fshift.par"))))
      (format nil "-Ffshift \"~a\"" tmpfile)))

(defmethod! om::supervp-frequencyshift ((self om::bpf))
    (let ((tmpfile (make-temp-param self (paramfile "fshift.par"))))
      (format nil "-Ffshift \"~a\"" tmpfile)))

(defmethod! om::supervp-frequencyshift ((self list))
    (let ((tmpfile (make-temp-param self (paramfile "fshift.par"))))
      (format nil "-Ffshift \"~a\"" tmpfile)))


;;;=================================================================================================
;;; BREAKPOINT-FILTER
;;;=================================================================================================

(defun breakpt-cmd (param extrapol)
  (if extrapol
      (om::string+ "-Fbreakpt " param)
    (om::string+ "-Fbreakpt-noex " param)))


(defmethod! om::supervp-breakpointfilter ((self list) extrapolation)
    :icon 951
    :menuins '((1 (("Extrapolation On" t) ("Extrapolation Off" nil))))
    :initvals '(((0.1 0 4 100 0 500 -30 1000 -30 4000 0)) t) 
    (let ((tmpfile (make-temp-param self (paramfile "breakpt.par"))))
      (breakpt-cmd (format nil "~s" tmpfile) extrapolation)))

(defmethod! om::supervp-breakpointfilter ((self pathname) extrapolation)
    (breakpt-cmd (format nil "\"~a\"" (namestring self)) extrapolation))

(defmethod! om::supervp-breakpointfilter ((self string) extrapolation)
    (breakpt-cmd (format nil "\"~a\"" 
                         (namestring (if (probe-file (pathname self)) self (paramfile self))))
                 extrapolation))

(defmethod! om::supervp-breakpointfilter ((self om::textbuffer) extrapolation)
    (let ((tmpfile (make-temp-param self (paramfile "breakpt.par"))))
      (breakpt-cmd (format nil "\"~a\"" tmpfile) extrapolation)))


;;;=================================================================================================
;;; FORMANT FILTER 
;;;=================================================================================================

(defun formant-cmd (param interpol extrapol)
  (if interpol
    (om::string+ (if extrapol "-Ffof " "-Ffof-noex ") param)
    (om::string+ (if extrapol "-Ffifof " "-Ffifof-noex ") param)))
    
(defmethod! om::supervp-formantfilter ((self list) interpolation extrapolation)
    :icon 951 
    :menuins '((1 (("Interpolation On" t) ("Interpolation Off" nil)))
               (2 (("Extrapolation On" t) ("Extrapolation Off" nil))))
    :initvals '(((0 2 350 0 50 1700 -5 100)) t t)

    (let ((tmpfile (make-temp-param self (paramfile "formant.par"))))
      (formant-cmd (format nil "\"~a\"" tmpfile) interpolation extrapolation)))
    

(defmethod! om::supervp-formantfilter ((self pathname) interpolation extrapolation)
    (formant-cmd (format nil "\"~a\"" (namestring self)) interpolation extrapolation))

  
(defmethod! om::supervp-formantfilter ((self string) interpolation extrapolation)
    (formant-cmd (format nil "\"~a\"" 
                         (namestring (if (probe-file (pathname self)) self (paramfile self))))
                 interpolation extrapolation))

(defmethod! om::supervp-formantfilter ((self om::textbuffer) interpolation extrapolation)
    (let ((tmpfile (make-temp-param self (paramfile "formant.par"))))
      (formant-cmd (format nil "\"~a\"" tmpfile) interpolation extrapolation)))

(defmethod! om::supervp-formantfilter ((self om::bpf) interpolation extrapolation)
    :icon 951 
    (let ((tmpfile (make-temp-param self (paramfile "formant.par"))))
      (formant-cmd (format nil "\"~a\"" tmpfile) interpolation extrapolation)))


;;;=================================================================================================
;;; BAND-FILTER
;;;=================================================================================================

(defun fband-cmd (param extrapol)
  (if extrapol
    (om::string+ "-Fbande " param)
    (om::string+ "-Fbande-noex " param)))


(defmethod! om::supervp-bandfilter  ((self list) extrapolation)
    :icon 951
    :menuins '((1 (("Extrapolation On" t) ("Extrapolation Off" nil))))
    :initvals '(((0 2 1 350 500)) t) 
    :doc "Band-Filter processing.

When <extrapolation>, SuperVP applies the treatment outside the time limits defined in the parameters file.
"
    (let ((tmpfile (make-temp-param self (paramfile "fband.par"))))
      (fband-cmd (format nil "\"~a\"" tmpfile) extrapolation)))

(defmethod! om::supervp-bandfilter  ((self pathname) extrapolation)
    (fband-cmd (format nil "\"~a\"" (namestring self)) extrapolation))

(defmethod! om::supervp-bandfilter ((self string) extrapolation)
    (fband-cmd (format nil "\"~a\"" 
                       (namestring (if (probe-file (pathname self)) self (paramfile self))))
               extrapolation))

(defmethod! om::supervp-bandfilter  ((self om::textbuffer) extrapolation)
    (let ((tmpfile (make-temp-param self (paramfile "fband.par"))))
      (fband-cmd (format nil "\"~a\"" tmpfile) extrapolation)))


;;;=================================================================================================
;;; CLIPING FILTER 
;;;=================================================================================================

(defun clipping-cmd (param renorm)
  (if renorm
    (om::string+ "-Fclip-norm " param)
    (om::string+ "-Fclip " param)))


(defmethod! om::supervp-clipping ((self pathname) renormalize)
    :icon 951
    :menuins '((1 (("Renormalize On" t) ("Renormalize Off" nil))))
    :initvals '(((-70 -60)) nil)
 
    (clipping-cmd (format nil "\"~a\"" (namestring self)) renormalize))

(defmethod! om::supervp-clipping ((self string) renormalize)
    (clipping-cmd (format nil "\"~a\"" 
                          (namestring (if (probe-file (pathname self)) self (paramfile self))))
                  renormalize))

(defmethod! om::supervp-clipping ((self om::textbuffer) renormalize)
    (let ((tmpfile (make-temp-param self (paramfile "clipping.par"))))
      (clipping-cmd (format nil "\"~a\"" tmpfile) renormalize)))

(defmethod! om::supervp-clipping ((self list) renormalize)
    (let ((tmpfile (make-temp-param self (paramfile "clipping.par"))))
      (clipping-cmd (format nil "\"~a\"" tmpfile) renormalize)))


;;;=================================================================================================
;;; FREEZE (Using SuperVP newfreeze)
;;;=================================================================================================

(defmethod! om::supervp-freeze ((self pathname))
    :icon 951
    :initvals '(((0.1 0.4 0.1)) nil) 
    (format nil "-Anewfreeze \"~a\"" (namestring self)))

(defmethod! om::supervp-freeze ((self string))
    (format nil "-Anewfreeze \"~a\"" 
            (namestring (if (probe-file (pathname self)) self (paramfile self)))))

(defmethod! om::supervp-freeze ((self om::textbuffer))
    (let ((tmpfile (make-temp-param self (paramfile "freeze.par"))))
      (format nil "-Anewfreeze \"~a\"" tmpfile)))

(defmethod! om::supervp-freeze ((self om::bpf))
    (let ((tmpfile (make-temp-param self (paramfile "freeze.par"))))
     (format nil "-Anewfreeze \"~a\"" tmpfile)))

(defmethod! om::supervp-freeze ((self list))
    (let ((tmpfile (make-temp-param self (paramfile "freeze.par"))))
      (format nil "-Anewfreeze \"~a\"" tmpfile)))


;;;=================================================================================================
;;; SURFACE FILTER 
;;;=================================================================================================

(defmethod! om::supervp-surfacefilter ((self pathname))
    :icon 951 
    (format nil "-Fsurface \"~a\"" (namestring self)))

(defmethod! om::supervp-surfacefilter ((self string))
    (format nil "-Fsurface \"~a\"" 
            (namestring (if (probe-file (pathname self)) self (paramfile self)))))

(defmethod! om::supervp-surfacefilter ((self om::textbuffer))
    (let ((tmpfile (make-temp-param self (paramfile "surfacefilter.par"))))
      (format nil "-Fsurface \"~a\"" tmpfile)))

(defmethod! om::supervp-surfacefilter ((self om::bpf))
    (let ((tmpfile (make-temp-param self (paramfile "surfacefilter.par"))))
     (format nil "-Fsurface \"~a\"" tmpfile)))

(defmethod! om::supervp-surfacefilter ((self list))
    (let ((tmpfile (make-temp-param self (paramfile "surfacefilter.par"))))
      (format nil "-Fsurface \"~a\"" tmpfile)))

;;;=================================================================================================
;;; FILE-POS
;;;=================================================================================================

(defmethod! om::supervp-filepos ((self pathname))
    :icon 951 
    (format nil "-Ipos \"~a\"" (namestring self)))

(defmethod! om::supervp-filepos ((self string))
   (format nil "-Ipos \"~a\"" 
           (namestring (if (probe-file (pathname self)) self (paramfile self)))))

(defmethod! om::supervp-filepos ((self om::textbuffer))
    (let ((tmpfile (make-temp-param self (paramfile "posfile.par"))))
      (format nil "-Ipos \"~a\"" tmpfile)))

(defmethod! om::supervp-filepos ((self list))
    (let ((tmpfile (make-temp-param self (paramfile "posfile.par"))))
      (format nil "-Ipos \"~a\"" tmpfile)))


;;;=================================================================================================
;;; GAIN / AMPLITUDE ENVELOPE
;;;=================================================================================================


(defmethod! om::supervp-gain ((self number))
    :icon 951 
    (format nil "-ggain ~f" self))

(defmethod! om::supervp-gain ((self pathname))
    (format nil "-ggain \"~a\"" (namestring self)))

(defmethod! om::supervp-gain ((self string))
    (format nil "-ggain \"~a\"" 
            (namestring (if (probe-file (pathname self)) self (paramfile self)))))

(defmethod! om::supervp-gain ((self om::textbuffer))
    (let ((tmpfile (make-temp-param self (paramfile "gain.par"))))
      (format nil "-ggain \"~a\""  tmpfile)))

(defmethod! om::supervp-gain ((self om::bpf))
    (let ((tmpfile (make-temp-param self (paramfile "gain.par"))))
     (format nil "-ggain \"~a\"" tmpfile)))

(defmethod! om::supervp-gain ((self list))
    (let ((tmpfile (make-temp-param self (paramfile "gain.par"))))
      (format nil "-ggain \"~a\"" tmpfile)))

