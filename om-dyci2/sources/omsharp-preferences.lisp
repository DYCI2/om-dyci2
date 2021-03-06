
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
(add-preference :libraries :dyci2-path "DYCI2 Modules folder" :file
                (merge-pathnames "lib/python/" (mypathname (find-library "om-dyci2")))
                "Locate load.py in DYCI2 Python Library" 'reinit-dyci2)
(add-preference :libraries ::dyci2-debug "Debug (open terminal output)" :action 'debug-dyci2)


(defmethod debug-dyci2 ()
  (listen *terminal-io*))


(defmethod reinit-dyci2 ()
  (when (file-exists-p (get-pref-value :libraries :dyci2-path))
    (dyci2::init-dyci2-lib (get-pref-value :libraries :dyci2-path))))

;;; do it at loading the lib...
(reinit-dyci2)
