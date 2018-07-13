
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
  (setf *DYCI2-PATH* (om-choose-file-dialog :prompt "Please locate the DYCI2 Modules loader")))

(defmethod! dyci2-path ()
  :icon '(186)
  *DYCI2-PATH*)

;;;=====================
;;; DYCI2 PREFS

(add-external-pref-module 'dyci2)

(defmethod get-external-name ((module (eql 'dyci2))) "DYCI2")
;; (defmethod get-external-icon ((module (eql 'dyci2))) (and (exist-lib-p "om-dyci2") (list 953 (exist-lib-p "om-dyci2"))))

;; (defmethod get-external-module-vals ((module (eql 'dyci2)) modulepref) (get-pref modulepref :dyci2-options))
(defmethod get-external-module-path ((module (eql 'dyci2)) modulepref) (get-pref modulepref :dyci2-path))
;; (defmethod set-external-module-vals ((module (eql 'dyci2)) modulepref vals) (set-pref modulepref :dyci2-options vals))
(defmethod set-external-module-path ((module (eql 'dyci2)) modulepref path) 
  (set-pref modulepref :dyci2-path path))


(defmethod save-external-prefs ((module (eql 'dyci2))) 
  `(:pm2-path ,(om-save-pathname *DYCI2-PATH*) 
    :pm2-options nil))

(defmethod put-external-preferences ((module (eql 'pm2)) moduleprefs)
  (let ((list-prefs (get-pref moduleprefs :dyci2-options)))
    (when list-prefs 
      nil ; pas d'options...
      )
    (when (get-pref moduleprefs :dyci2-path)
      (setf *DYCI2-PATH* (om-make-pathname :directory (get-pref moduleprefs :dyci2-path)))
      (reinit-dyci2))
    ))

(put-external-pref-values 'dyci2)
