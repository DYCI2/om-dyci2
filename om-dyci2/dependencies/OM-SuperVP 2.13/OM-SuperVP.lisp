;;;===================================================
;;;
;;; OM-SuperVP
;;; SuperVP sound analysis and processing for OpenMusic
;;;
;;;
;;; LIBRARY MAIN FILE
;;; Authors: Jean Bresson, Jean Lochard (IRCAM - 2006-2010)
;;;
;;;===================================================

(om-get-picture "superVP" 'om-supervp)

(in-package :om)

;--------------------------------------------------
;Variable definiton with files to load 
;--------------------------------------------------

;;; removes extension
(defun lib-src-file (file)
  (merge-pathnames file (om-make-pathname :directory *load-pathname*)))

;--------------------------------------------------
;Loading files 
;--------------------------------------------------

(mapc #'(lambda (file) (compile&load (lib-src-file file) t t))
      '("sources/package"
        "sources/general"
        "sources/normalize"
        "sources/analysis"
        "sources/processing"
        "sources/synthesis"
        "sources/channels"
        "sources/formants"
        "sources/om-preferences"
        ))

;--------------------------------------------------
; OM subpackages initialization
; ("sub-pack-name" subpacke-lists class-list function-list class-alias-list)
;--------------------------------------------------
(om::fill-library  
 '(("Analysis" nil nil (fft f0-estimate transient-detection formant-analysis) nil)
   ("Treatments" nil nil (supervp-transposition supervp-timestretch
                                                supervp-frequencyshift supervp-breakpointfilter
                                                supervp-formantfilter supervp-bandfilter
                                                supervp-clipping supervp-freeze
                                                supervp-surfacefilter
                                                supervp-gain) nil)
        
   ("Processing" nil nil (supervp-processing 
                          supervp-cross-synthesis 
                          supervp-sourcefilter-synthesis
                          supervp-normalize supervp-merge supervp-split) nil)
   ("ASX" nil nil (asx::trans-melody))
   ("Tools" nil nil (make-spec-env) nil)
   )
 (find-library "OM-SuperVP"))



(doc-library "OM-SuperVP is a library for the control of SuperVP from OpenMusic.

It contains functions for sound analysis (FFT, Transients, Fundamental frequency, Formants) and processing (Time/Frequency transformations, filters, cross-synthesis, etc.).

The library requires SuperVP to be installed, authorized, and declared as an OM external (see Preferences/Extrernals).
SuperVP can also be selected as a sound normalizer in OM sound processing (see Preferences/Audio).

" 
             (find-library "OM-SuperVP"))

; (gen-lib-reference (find-library "OM-SuperVP"))

(unless (fboundp 'om::set-lib-release) (defmethod om::set-lib-release (version &optional lib) nil))


(om::set-lib-release 2.13)

;;; compat
(defun om::om-directory-pathname-p (p) (om::directoryp p))



(print "
;;;==========================================
;;; OM-SuperVP
;;; SuperVP sound analysis and processing for OpenMusic
;;;
;;; J. Bresson, J. Lochard, IRCAM (2006-2018)
;;;==========================================
")



;;;==================================
;;; fix a compatibility issue
;;;==================================
(defmethod update-inputs ((ref (eql 'transient-detection)) inputs) 
  (let ((t-in (find "treshold" inputs :key 'name :test 'string-equal)))
    (when t-in (setf (name t-in) "threshold")))
  (call-next-method))


