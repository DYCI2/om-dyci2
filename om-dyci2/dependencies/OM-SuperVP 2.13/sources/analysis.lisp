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
; SOUND ANALYSIS TOOLS
; File author: J. Bresson (IRCAM - 2006-2016)
;============================================================================


(in-package :svp)


;;;================================================================================================================
;;; FFT
;;;================================================================================================================

(defmethod! om::fft ((in string) &key begin-time end-time (windowsize 4096) (fftsize 4096) (step 256) 
                 (windowtype "hanning") (gabarit 1) (out "fft.sdif"))
  :initvals '(nil nil nil 4096 4096 256 "hanning" 1 "fft.sdif")
  :menuins (list (list 6 '(("Blackman" "blackman") ("Hanning" "hanning") ("Hamming" "hamming")))
                 (list 7 '(("log amplitude" 0) ("amplitude" 1) ("phase" 2) ("phase / amplitude" 3) 
                           ("frequency / amplitude" 4) ("real / imaginary" 5))))
  :icon 952
  :doc "Calculates the FFT of a Sound file using SuperVP.
The results of analysis are stored in an SDIF file which pathname is returned.

- <infile> : pathname or SOUND object to be analysed

- <begin-time> : begin time of the analysis

- <end-time> : end time of the analysis

- <windowsize> : number of samples of the analysis window

- <fftsize> : number of points in fft

- <step> : number of samples between two successive analysis windows

- <windowtype> : shape of the analysis window

- <gabarit> : output fft format
            0 = logarithmic amplitudes
            1 = amplitude
            2 = phase
            3 = phase, amplitude
            4 = frequency, amplitude
            5 = real, imaginary 
Note that only sdiffiles at gabarit 3 and 5 can be used to recalculate a sound file using IFFT.

- <out> : output file pathname
"

  (let ((supervp-path (om::svp-path)))
    (if (and supervp-path (probe-file supervp-path))
        (let ((outname (namestring (if out
                                       (om::handle-new-file-exists (if (pathnamep out) out (om::outfile out)))
                                     (om::om-choose-new-file-dialog :prompt "Choose a new SDIF FFT file"
                                                                    :directory (om::def-save-directory))))))
          (when outname
            (setf om::*last-saved-dir* (make-pathname :directory (pathname-directory outname)))
            (let* ((beginstr (if begin-time (format nil "-B~D " begin-time) ""))
                   (endstr (if end-time (format nil "-E~D " end-time) ""))
                   (fftstr (format nil "-N~D -M~D -W~D -I~D " fftsize windowsize windowtype step))
                   (ostr (format nil "-OS~D " (if (and gabarit (>= gabarit 0) (<= gabarit 5)) gabarit 
                                                (progn (om::om-message-dialog (format nil "FFT gabarit ~D does not exist.~% FFT will be formated at gabarit 1" gabarit))
                                                  1)))) 
                   (cmd (format nil "~s -v -t -S~s -Afft -U ~A ~A ~A ~s ~s" 
                                (namestring supervp-path)
                                (namestring in)
                                fftstr beginstr endstr ostr outname))
                   )
              (print "==========================")
              (print "SUPERVP FFT ANALYSIS")
              (print "==========================")
              (om::om-cmd-line cmd)
              (and outname (probe-file outname)))))
      (om::om-beep-msg "SuperVP not found! Set path to SuperVP in the OM preferences."))
    ))

(defmethod! om::fft ((in pathname) &key begin-time end-time (windowsize 4096) (fftsize 4096) (step 256) 
                     (windowtype "hanning") (gabarit 1)  (out "fft.sdif"))
  (om::fft (namestring in) 
       :begin-time begin-time :end-time end-time 
       :fftsize fftsize :windowsize windowsize
       :step step :windowtype windowtype :gabarit gabarit 
       :out out))

(defmethod! om::fft ((in sound) &key begin-time end-time (windowsize 4096) (fftsize 4096) (step 256) 
                 (windowtype "hanning") (gabarit 1) (out "fft.sdif"))
  (if (om::om-sound-file-name in)
      (om::fft (om::om-sound-file-name in) 
               :begin-time begin-time :end-time end-time 
               :fftsize fftsize :windowsize windowsize
               :step step :windowtype windowtype :gabarit gabarit 
               :out out)
    (om::om-beep-msg "FFT: input sound file must be saved on disk!")))



;;;================================================================================================================
;;; TRUE ENVELOPE
;;;================================================================================================================

(defmethod! om::true-envelope ((in string) &key begin-time end-time (windowsize 4096) (fftsize 4096) (step 256) 
                 (windowtype "hanning") (max-fun-freq 1000) (gabarit 1) (out "true_envelope.sdif"))
  :initvals '(nil nil nil 4096 4096 256 "hanning" 1000 1 "true_envelope.sdif")
  :menuins (list (list 6 '(("Blackman" "blackman") ("Hanning" "hanning") ("Hamming" "hamming")))
                 (list 7 '(("log amplitude" 0) ("amplitude" 1) ("phase" 2) ("phase / amplitude" 3) 
                           ("frequency / amplitude" 4) ("real / imaginary" 5))))
  :icon 952
  :doc "Calculates the FFT of a Sound file using SuperVP.
The results of analysis are stored in an SDIF file which pathname is returned.

- <infile> : pathname or SOUND object to be analysed

- <begin-time> : begin time of the analysis

- <end-time> : end time of the analysis

- <windowsize> : number of samples of the analysis window

- <fftsize> : number of points in fft

- <step> : number of samples between two successive analysis windows

- <windowtype> : shape of the analysis window

- <max-fund-freq> : maximum for the fundamental frequency (Hz)

- <gabarit> : output fft format
            0 = logarithmic amplitudes
            1 = amplitude
            2 = phase
            3 = phase, amplitude
            4 = frequency, amplitude
            5 = real, imaginary 
Note that only sdiffiles at gabarit 3 and 5 can be used to recalculate a sound file using IFFT.

- <out> : output file pathname
"

  (let ((supervp-path (om::svp-path)))
    (if (and supervp-path (probe-file supervp-path))
        (let ((outname (namestring (if out
                                       (om::handle-new-file-exists (if (pathnamep out) out (om::outfile out)))
                                     (om::om-choose-new-file-dialog :prompt "Choose a new SDIF file"
                                                                    :directory (om::def-save-directory))))))
          (when outname
            (setf om::*last-saved-dir* (make-pathname :directory (pathname-directory outname)))
            (let* ((beginstr (if begin-time (format nil "-B~D " begin-time) ""))
                   (endstr (if end-time (format nil "-E~D " end-time) ""))
                   (fundstr (format nil "~DHz" max-fun-freq)) ; main difference with FFT
                   (fftstr (format nil "-N~D -M~D -W~D -I~D " fftsize windowsize windowtype step))
                   (ostr (format nil "-OS~D " (if (and gabarit (>= gabarit 0) (<= gabarit 5)) gabarit 
                                                (progn (om::om-message-dialog (format nil "FFT gabarit ~D does not exist.~% FFT will be formated at gabarit 1" gabarit))
                                                  1)))) 
                   (cmd (format nil "~s -v -t -S~s -Atenv ~A -U ~A ~A ~A ~s ~s" 
                                (namestring supervp-path)
                                (namestring in)
                                fundstr
                                fftstr beginstr endstr ostr outname))
                   )
              (print "==============================")
              (print "SUPERVP TRUE ENVELOPE ANALYSIS")
              (print "==============================")
              (om::om-cmd-line cmd)
              (and outname (probe-file outname)))))
      (om::om-beep-msg "SuperVP not found! Set path to SuperVP in the OM preferences."))
    ))

(defmethod! om::true-envelope ((in pathname) &key begin-time end-time (windowsize 4096) (fftsize 4096) (step 256) 
                     (windowtype "hanning") (max-fun-freq 1000) (gabarit 1) (out "true_envelope.sdif"))
  (om::true-envelope (namestring in) 
       :begin-time begin-time :end-time end-time 
       :fftsize fftsize :windowsize windowsize
       :step step :windowtype windowtype 
       :max-fun-freq max-fun-freq
       :gabarit gabarit :out out))

(defmethod! om::true-envelope ((in sound) &key begin-time end-time (windowsize 4096) (fftsize 4096) (step 256) 
                 (windowtype "hanning") (max-fun-freq 1000) (gabarit 1) (out "true_envelope.sdif"))
  (if (om::om-sound-file-name in)
      (om::true-envelope (om::om-sound-file-name in) 
               :begin-time begin-time :end-time end-time 
               :fftsize fftsize :windowsize windowsize
               :step step :windowtype windowtype 
               :max-fun-freq max-fun-freq
               :gabarit gabarit :out out)
    (om::om-beep-msg "FFT: input sound file must be saved on disk!")))




;;;================================================================================================================
;;; FUNDAMENTAL FREQUENCY ESTIMATE
;;;================================================================================================================

(defmethod! om::f0-estimate ((in string) &key begin-time end-time 
                         (fund-minfreq 50.0) (fund-maxfreq 1000.0) (spectrum-maxfreq 4000.0) (noise-threshold 50.0) (smooth-order 3)
                         (windowsize 4096) (fftsize 4096) (step 256) (windowtype "hanning") (out "f0.sdif"))
  :initvals '(nil nil nil 50.0 1000.0 4000.0 50.0 3 4096 4096 256 "hanning" "f0.sdif")
  :menuins (list (list 11 '(("Blackman" "blackman")("Hanning" "hanning")("Hamming" "hamming"))))
  :icon 952
  :doc "Calculates the Fundamental Fequency in <FileName> using SuperVP.
The results of analysis are stored in an SDIF file which pathname is returned.

- <infile> : pathname or SOUND object to be analysed

- <begin-time> : the begin time of the analysis (s)
 
- <end-time> : the end time of the analysis (s)

- <fund-minfreq> : Fundamental minimal frequency (Hz)

- <fund-maxfreq> : Fundamental maximal frequency (Hz)

- <spectrum-maxfreq> : Maximal frequency in spectrum (Hz)

- <noise-threshold> : Noise threshold (dB)

- <smooth-order> : Smooth Order (int)

- <windowsize> : number of samples of the analysis window

- <fftsize> : number of points of fft

- <step> : number of samples between two successive analysis windows

- <windowtype> : shape of the analysis window

- <out> : output file pathname

"
  (let ((supervp-path (om::svp-path)))
    (if (and supervp-path (probe-file supervp-path))
        (let ((outname (namestring (if out 
                           (om::handle-new-file-exists (if (pathnamep out) out (om::outfile out)))
                         (om::om-choose-new-file-dialog :prompt "Choose a new SDIF F0 file"
                                                    :directory (om::def-save-directory))))))
          (when outname
            (setf om::*last-saved-dir* (make-pathname :directory (pathname-directory outname)))
            (let* ((beginstr (if begin-time (format nil "-B~D " begin-time) ""))
                   (endstr (if end-time (format nil "-E~D " end-time) ""))
                   (fftstr (format nil "-N~D -M~D -W~D -I~D " fftsize windowsize windowtype step))
                   (f0params (format nil "fm~D fM~D F~D sn~D smooth~D " 
                                     fund-minfreq fund-maxfreq spectrum-maxfreq noise-threshold smooth-order))
                   (cmd (format nil "~s -v -t -ns -U -S~s -Af0 ~s ~A ~A ~A -OS0 ~s" 
                                (namestring supervp-path)
                                (namestring in)
                                f0params
                                fftstr beginstr endstr outname))
              
                   )
              (print "===========================")
              (print "SUPERVP F0 ANALYSIS")
              (print "===========================")
              (print cmd)   
              (om::om-cmd-line cmd)
              (and outname (probe-file outname)))))
      (om::om-beep-msg "SuperVP not found! Set path to SuperVP in the OM preferences."))
    ))


(defmethod! om::f0-estimate ((in pathname) &key begin-time end-time 
                         (fund-minfreq 50.0) (fund-maxfreq 1000.0) (spectrum-maxfreq 4000.0) (noise-threshold 50.0) (smooth-order 3)
                         (windowsize 4096) (fftsize 4096) (step 256) (windowtype "hanning") (out "f0.sdif"))
  (om::f0-estimate (namestring in) 
               :begin-time begin-time :end-time end-time 
               :fund-minfreq fund-minfreq :fund-maxfreq fund-maxfreq :spectrum-maxfreq spectrum-maxfreq 
               :noise-threshold noise-threshold :smooth-order smooth-order
               :windowsize windowsize :fftsize fftsize :step step :windowtype windowtype 
               :out out))

(defmethod! om::f0-estimate ((in sound) &key begin-time end-time 
                         (fund-minfreq 50.0) (fund-maxfreq 1000.0) (spectrum-maxfreq 4000.0) (noise-threshold 50.0) (smooth-order 3)
                         (windowsize 4096) (fftsize 4096) (step 256) (windowtype "hanning") (out "f0.sdif"))  
  (if (om::om-sound-file-name in)
      (om::f0-estimate (om::om-sound-file-name in) 
               :begin-time begin-time :end-time end-time 
               :fund-minfreq fund-minfreq :fund-maxfreq fund-maxfreq :spectrum-maxfreq spectrum-maxfreq 
               :noise-threshold noise-threshold :smooth-order smooth-order
               :windowsize windowsize :fftsize fftsize :step step :windowtype windowtype 
               :out out)
    (om::om-beep-msg "F0-ESTIMATE: input sound file must be saved on disk!")))

;;;================================================================================================================
;;; TRANSIENT DETECTION : GENERATE MARKERS
;;;================================================================================================================

(defmethod! om::transient-detection ((in string) &key
                                 (threshold 1.4) (minfreq 0.0) (maxfreq 22050.0) (minoffset 0.02)
                                 (windowsize 4096) (fftsize 4096) (step-oversamp 8) (windowtype "hanning") 
                                 (out "markers.sdif"))
  :initvals '(nil 1.4 0.0 22050.0 0.02 4096 4096 8 "hanning" "markers.sdif")
  :menuins (list (list 7 '(("1/4" 4) ("1/8" 8) ("1/16" 16) ("1/32" 32)))
                 (list 8 '(("Blackman" "blackman")("Hanning" "hanning")("Hamming" "hamming"))))
  :icon 952
  :doc "Detects transients in <filename> using SuperVP.
The results of analysis are stored in an SDIF file which pathname is returned.

- <infile> : pathname or SOUND object to be analysed

- <threshold> : transient detection treshold (1-10)

- <minfreq> : minimal detection frequency (Hz)

- <maxfreq< : maximal detection frequency (Hz)

- <minoffset> : min offset between markers (s)

- <windowsize> : number of samples of the analysis window

- <fftsize> : number of points of fft

- <step-oversamp> : oversampling proportion between two successive analysis windows

- <windowtype> : shape of the analysis window

- <out> : output file pathname
"
  (let ((supervp-path (om::svp-path)))
    (if (and supervp-path (probe-file supervp-path))
        (let ((outname (namestring (if out 
                                       (om::handle-new-file-exists (if (pathnamep out) out (om::outfile out)))
                                     (om::om-choose-new-file-dialog :prompt "Choose a new SDIF markers file"
                                                                    :directory (om::def-save-directory))))))
          (setf om::*last-saved-dir* (make-pathname :directory (pathname-directory outname)))
          (when outname
            (let* ((fftstr (format nil "-N~D -M~D -W~D -oversamp ~D " fftsize windowsize windowtype step-oversamp))
                   (transient-params (format nil "-OT -td_thresh ~D -td_G 2.5 -td_band ~D,~D -td_nument 10.0 -td_minoff ~Ds"
                                             threshold minfreq maxfreq minoffset))
                   (cmd (format nil "~s -v -t -ns -S~s -Afft ~A ~A ~s" 
                                (namestring supervp-path)
                                (namestring in)
                                fftstr transient-params
                                outname))
                   )
              (print "===========================")
              (print "SUPERVP TRANSIENT DETECTION")
              (print "===========================")
              (print cmd)   
              (om::om-cmd-line cmd)
              (and outname (probe-file outname)))))
      (om::om-beep-msg "SuperVP not found! Set path to SuperVP in the OM preferences."))
    ))


(defmethod! om::transient-detection ((in pathname) &key
                         (threshold 1.4) (minfreq 0.0) (maxfreq 22050.0) (minoffset 0.02)
                         (windowsize 4096) (fftsize 4096) (step-oversamp 8) (windowtype "hanning") 
                          (out "markers.sdif"))
  (om::transient-detection (namestring in) 
               :threshold threshold :minfreq minfreq :maxfreq maxfreq :minoffset minoffset
               :windowsize windowsize :fftsize fftsize :step-oversamp step-oversamp :windowtype windowtype 
               :out out))

(defmethod! om::transient-detection ((in sound) &key 
                         (threshold 1.4) (minfreq 0.0) (maxfreq 22050.0) (minoffset 0.02)
                         (windowsize 4096) (fftsize 4096) (step-oversamp 8) (windowtype "hanning") 
                         (out "markers.sdif"))
  (if (om::om-sound-file-name in)
      (om::transient-detection (om::om-sound-file-name in) 
                       :threshold threshold :minfreq minfreq :maxfreq maxfreq :minoffset minoffset
                       :windowsize windowsize :fftsize fftsize :step-oversamp step-oversamp :windowtype windowtype 
                       :out out)
  (om::om-beep-msg "TRANSIENT-DETECTION: input sound file must be saved on disk!")))  


;;;================================================================================================================
;;; FORMANT ANALYSIS
;;;================================================================================================================

(defmethod! om::formant-analysis ((in string) &key
                              (analysis-mode 'lpc) (max-n-formants 45) (analysis-order 45) (extend-across-sound-limits nil)
                              (windowsize 2048) (fftsize 2048) (step-oversamp 8) (windowtype "blackman") 
                              (out "formants.sdif"))
  :initvals '(nil lpc 45 45 nil 2048 2048 8 "blackman" "formants.sdif")
  :menuins '((1 (("LPC" lpc) ("Discrete Spectrum" ds)))
             (4 (("No" nil) ("Yes" t)))
             (7 (("1/4" 4) ("1/8" 8) ("1/16" 16) ("1/32" 32)))
             (8 (("Blackman" "blackman") ("Hanning" "hanning") ("Hamming" "hamming"))))
  :icon 952
  :doc "Performs formant analysis of a sound using SuperVP.
The results of analysis are stored in an SDIF file which pathname is returned.

- <infile> the pathname or SOUND object to be analysed

- <analysis-mode> : Type of analysis (LPC or Discrete Spectrum)

- <analysis-order> : order of analysis

- <extend-across-sound-limits> determines whether the first analysis window is centered over the first sample (T) or starts at the first sample (NIL).

- <windowsize> : number of samples of the analysis window

- <fftsize> : number of points of fft

- <step-oversamp> : oversampling proportion between two successive analysis windows

- <windowtype> : shape of the analysis window

- <out> : output file pathname

"
  (let ((supervp-path (om::svp-path)))
    (if (and supervp-path (probe-file supervp-path))
        (let ((outname (namestring (if out 
                                       (om::handle-new-file-exists (if (pathnamep out) out (om::outfile out)))
                                     (om::om-choose-new-file-dialog :prompt "Choose a new SDIF file"
                                                                :directory (om::def-save-directory))))))
              (setf om::*last-saved-dir* (make-pathname :directory (pathname-directory outname)))
              (when outname
                (let* ((fftstr (format nil "-N~D -M~D -W~D -oversamp ~D" fftsize windowsize windowtype step-oversamp))
                       (formantsstr (om::string+ 
                                     (if (equal analysis-mode 'ds) "-Aformant_ced" "-Aformant_lpc")
                                     " n" (om::number-to-string max-n-formants) " " (om::number-to-string analysis-order)))
                       (uflagstr (if extend-across-sound-limits "" "-U"))
                       (cmd (format nil "~s -v -t -ns ~A -S~s ~A ~A -OS0 ~s" 
                                    (namestring supervp-path)
                                    uflagstr
                                    (namestring in)
                                    fftstr formantsstr
                                    outname))
                       )
                  (print "==========================")
                  (print "SUPERVP FORMANT ANALYSIS")
                  (print "==========================")
                  (om::om-cmd-line cmd)
                  (and outname (probe-file outname)))))
      (om::om-beep-msg "SuperVP not found! Set path to SuperVP in the OM preferences."))
    ))


(defmethod! om::formant-analysis ((in pathname) &key
                              (analysis-mode 'lpc) (max-n-formants 45) (analysis-order 45) (extend-across-sound-limits nil)
                              (windowsize 2048) (fftsize 2048) (step-oversamp 8) (windowtype "blackman") 
                              (out "formants.sdif"))
  (om::formant-analysis (namestring in)
               :analysis-mode analysis-mode :max-n-formants max-n-formants :analysis-order analysis-order 
               :extend-across-sound-limits extend-across-sound-limits 
               :windowsize windowsize :fftsize fftsize :step-oversamp step-oversamp :windowtype windowtype 
               :out out))

(defmethod! om::formant-analysis ((in sound) &key 
                              (analysis-mode 'lpc) (max-n-formants 45) (analysis-order 45) (extend-across-sound-limits nil)
                              (windowsize 2048) (fftsize 2048) (step-oversamp 8) (windowtype "blackman") 
                              (out "formants.sdif"))
   (if (om::om-sound-file-name in)
       (om::formant-analysis (om::om-sound-file-name in) 
                              :analysis-mode analysis-mode :max-n-formants max-n-formants :analysis-order analysis-order 
                              :extend-across-sound-limits extend-across-sound-limits 
                              :windowsize windowsize :fftsize fftsize :step-oversamp step-oversamp :windowtype windowtype 
                              :out out)
     (om::om-beep-msg "FORMANT-ANALYSIS: input sound file must be saved on disk!"))  )

