
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
; Authors: Victoire Siguret, Jean Bresson, Jérôme Nika - IRCAM/STMS 2018
;============================================================================

(in-package :om)

(add-preference-section :libraries "DYCI2" nil '(:dyci2-path :dyci2-debug))
(add-preference :libraries :dyci2-path "DYCI2 Modules folder" :folder nil "Point to the \"Dyci2_Modules\" folder in the DYCI2 library" 'reinit-dyci2) 
(add-preference :libraries ::dyci2-debug "Debug (open terminal output)" :action 'debug-dyci2)

;;; works for pm2...
(defun forum-authorize (exe-path)
  (if exe-path
    (let ((auth-file (om-choose-file-dialog :prompt "Pleas select the .txt file provided by the ForumNet code generator")))
      (when (and auth-file (probe-file auth-file))
        (om-cmd-line (format nil "~s -init_key_file ~s" 
                             (namestring (real-exec-pathname exe-path))
                             (namestring auth-file)))
        (print "Authorization... done")))
    (om-beep-msg "Executable not found: ~A" exe-path)
  ))

(defmethod! debug-dyci2 ()
   (listen *terminal-io*))

(defmethod! reinit-dyci2 ()
  (dyci2::init-dyci2-lib (get-pref-value :libraries :dyci2-path)))