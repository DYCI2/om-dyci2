
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

(defvar *DYCI2-PATH* nil)

(defmethod! reinit-dyci2 ()
  (dyci2::init-dyci2-lib *DYCI2-PATH*))

(defmethod! set-dyci2-path ((path string))
  :icon '(186)
  (set-dyci2-path (pathname path)))

(defmethod! set-dyci2-path ((path pathname))
  (setf *DYCI2-PATH* path)
  (reinit-dyci2))

(defmethod! set-dyci2-path ((path null))
  (setf *DYCI2-PATH* (om-choose-file-dialog :prompt "Please locate the DYCI2 load module.")))

(defmethod! dyci2-path ()
  :icon '(186)
  *DYCI2-PATH*)

;;;=====================
;;; PREFERENCES
;;;=====================

(add-external-pref-module 'dyci2)

(defmethod get-external-name ((module (eql 'dyci2))) "DYCI2")

(defmethod get-external-module-path ((module (eql 'dyci2)) modulepref)
  (get-pref modulepref :dyci2-path))

(defmethod set-external-module-path ((module (eql 'dyci2)) modulepref path)
  (set-pref modulepref :dyci2-path path))


(defmethod save-external-prefs ((module (eql 'dyci2)))
  `(:dyci2-path ,(om-save-pathname *DYCI2-PATH*)
    :dyci2-options nil))

(defmethod put-external-preferences ((module (eql 'dyci2)) moduleprefs)
  (let ((list-prefs (get-pref moduleprefs :dyci2-options)))
    (when list-prefs
      nil ; no options (yet..)
      )
    (when (get-pref moduleprefs :dyci2-path)
      (setf *DYCI2-PATH* (get-pref moduleprefs :dyci2-path))
      (reinit-dyci2))
    ))

(put-external-pref-values 'dyci2)
