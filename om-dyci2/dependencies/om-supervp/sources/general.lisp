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
; GENERAL SUPERVP CALL 
; File author: Jean Bresson (IRCAM - 2006-2016)
;============================================================================


(in-package :svp)
  
(defmethod! om::supervp-command ((cmd string))
  :icon 950  
  (let ((supervp-path (om::real-exec-pathname (om::get-pref-value :libraries :supervp-path))))
    (if (and supervp-path (probe-file supervp-path))
        (om::om-cmd-line (format nil "~s ~A" (namestring supervp-path) cmd)) 
      (om::om-beep-msg "SuperVP not found! Set path to SuperVP in the OM preferences."))))


