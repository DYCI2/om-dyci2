;;;===================================================
;;;
;;; OM-SuperVP
;;; SuperVP sound analysis and processing for OpenMusic
;;;
;;; Requires perliminary installation of SuperVP (also included in AudioSculpt)
;;; Set SuperVP path in OM preferences (externals) once the library is loaded.
;;;
;;;
;;; SUPERVP PREFERENCES
;;; Integrated in the 'Externals' tab in OM preferences
;;; Authors: Jean Bresson (IRCAM - 2006-2010)
;;;
;;;===================================================
; PREFERENCES FOR OM6
; File author: J. Bresson (IRCAM - 2006-2010)
;=====================================================


(in-package :om)


(defvar *SVP-PATH* "path to svp")
(defvar *param-files-folder* nil)

(defvar *delete-param-files* t)


;;;== PARAM FILES ===

(defmethod! set-svp-paramfiles-folder ((path string))
  :icon 186
  (setf *param-files-folder* (pathname path)))

(defmethod! set-svp-paramfiles-folder ((path pathname))
  :icon 186
  (setf *param-files-folder* path))

(defmethod! svp-paramfile ((name string))
  :icon 186
  (make-pathname :directory (pathname-directory *param-files-folder*) :name name
                 :host (pathname-host *param-files-folder*) 
                 :device (pathname-device *param-files-folder*)))

(defmethod! svp-paramfile ((name null)) *param-files-folder*)


;;;=== SVP PATH ===

(defmethod! set-svp-path ((path string))
  :icon 186
  (setf *SVP-PATH* (pathname path)))

(defmethod! set-svp-path ((path pathname))
  :icon 186
  (setf *SVP-PATH* path))

(defmethod! svp-path ()
  :icon 186
  (namestring *SVP-PATH*))



;;;=====================
;;; SVP PREFS

(add-external-pref-module 'svp)




(defmethod get-external-name ((module (eql 'svp))) "SuperVP")

(defmethod get-external-module-vals ((module (eql 'svp)) modulepref) (get-pref modulepref :svp-options))
(defmethod get-external-module-path ((module (eql 'svp)) modulepref) (get-pref modulepref :svp-path))
(defmethod set-external-module-vals ((module (eql 'svp)) modulepref vals) (set-pref modulepref :svp-options vals))
(defmethod set-external-module-path ((module (eql 'svp)) modulepref path) 
  (set-pref modulepref :svp-path path))


;;; params folder
(defun def-svp-options ()
  (list *om-infiles-folder*))

(defmethod get-external-def-vals ((module (eql 'svp))) 
   (let ((libpath (lib-pathname (find-library "OM-SuperVP"))))
     (list :svp-path (om-make-pathname :directory (append (pathname-directory libpath) 
                                                          '("bin")
                                                          #+macosx '("mac" "SuperVP.app" "Contents" "MacOS") 
                                                          #+win32 '("win")
                                                          #+linux '("linux")
                                                          )
                                       :host (pathname-host libpath) :device (pathname-device libpath)
                                        :name "supervp" #+win32 :type #+win32 "exe")
           :svp-options (def-svp-options))))

(defmethod save-external-prefs ((module (eql 'svp))) 
  `(:svp-path ,(om-save-pathname *SVP-PATH*) 
    :svp-options (list ,(om-save-pathname *param-files-folder*))))


(defmethod put-external-preferences ((module (eql 'svp)) moduleprefs)
  (let ((list-prefs (get-pref moduleprefs :svp-options)))
    (when list-prefs 
      (if (probe-file (nth 0 list-prefs))
       (setf *param-files-folder* (nth 0 list-prefs))
       (progn
         (setf (nth 0 list-prefs) (nth 0 
                                       (nth (+ 1 (position :svp-options (get-external-def-vals module)))
                                            (get-external-def-vals module))))
         (set-pref moduleprefs :svp-options list-prefs)
         ;(push :svp-params-dir *restore-defaults*)
         ))
      )
    (when (get-pref moduleprefs :svp-path)
      (setf *SVP-PATH* (find-true-external (get-pref moduleprefs :svp-path))))
    ))

(put-external-pref-values 'svp)


(defun forum-authorize (exe-path)
  (let ((auth-file (om-choose-file-dialog :prompt "Pleas select the .txt file provided by the ForumNet code generator")))
    (when (and auth-file (probe-file auth-file))
      (om-cmd-line (format nil "~s -init_key_file ~s" 
                           (namestring (find-true-external exe-path))
                           (namestring auth-file))
                   t t)
      (print "Authorization... done")
      )))

(defmethod show-external-prefs-dialog ((module (eql 'svp)) prefvals)
  (let* ((rep-list (copy-list prefvals))
         (dialog (om-make-window 'om-dialog
                                 :window-title "SuperVP Options"
                                 :size (om-make-point 300 200)
                                 :position :centered
                                 :resizable nil :maximize nil :close nil))
         paramstext
         (i 0))
    (om-add-subviews dialog
                     (om-make-dialog-item 'om-static-text  (om-make-point 20 (incf i 20)) (om-make-point 200 20) 
                                          "SuperVP Forum Activation" :font *om-default-font1*)

                     (om-make-dialog-item 'om-button (om-make-point 180 (- i 4)) (om-make-point 50 20) "Go!" 
                                          :di-action (om-dialog-item-act item
                                                       (declare (ignore item))
                                                       (if (probe-file *svp-path*) (forum-authorize *svp-path*)
                                                         (om-message-dialog "Please set SuperVP path first!"))))
                     
                     (om-make-dialog-item 'om-static-text  (om-make-point 20 (incf i 55)) (om-make-point 300 20) 
                                          "Param Files folder (read/write):" :font *om-default-font1b*)
                     (setf paramstext (om-make-dialog-item 'om-static-text  (om-make-point 20 (1+ (incf i 16))) (om-make-point 220 50) 
                                                           (om-namestring (nth 0 prefvals))
                                                           :font *om-default-font1*))
                     (om-make-dialog-item 'om-button (om-make-point 240 i) (om-make-point 50 24) "..." 
                                          :di-action (om-dialog-item-act item
                                                       (declare (ignore item))
                                                       (let ((newpath (om-choose-directory-dialog 
                                                                       :directory (om-namestring (nth 0 prefvals)))))
                                                         (when newpath 
                                                           (om-set-dialog-item-text paramstext (om-namestring newpath))
                                                           ))))
                     ;;; boutons
                     (om-make-dialog-item 'om-button (om-make-point 20 (incf i 66)) (om-make-point 80 20) "Restore"
                                          :di-action (om-dialog-item-act item
                                                       (om-set-dialog-item-text paramstext (namestring (nth 0 (def-svp-options))))
                                                       ))
      
      (om-make-dialog-item 'om-button (om-make-point 130 i) (om-make-point 80 20) "Cancel"
                           :di-action (om-dialog-item-act item
                                        (om-return-from-modal-dialog dialog nil)))
      
      (om-make-dialog-item 'om-button (om-make-point 210 i) (om-make-point 80 20) "OK"
                           :di-action (om-dialog-item-act item
                                        (let ((argerror nil)
                                              (partxt (om-dialog-item-text paramstext)))
                                          (setf (nth 0 rep-list) partxt)
                                          (if argerror
                                              (om-message-dialog (format nil "Error in a SuperVP option.~% Preferences could not be recorded.")) 
                                            (om-return-from-modal-dialog dialog rep-list))
                                          ))
                           :default-button t :focus t)
                     
      )
    (om-modal-dialog dialog)))









