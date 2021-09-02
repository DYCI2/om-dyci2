;============================================================================
; OM-SuperVP
; SuperVP sound analysis and processing for om#
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
; PREFERENCES FOR om#
; File author: J. Bresson (IRCAM - 2017)
;============================================================================


(in-package :om)

(defmethod default-supervp-path () 
  (let ((libpath (mypathname (find-library "om-supervp"))))
    (om-make-pathname :directory (append (pathname-directory libpath) 
                                         '("bin")
                                         #+macosx '("mac" "SuperVP.app" "Contents" "MacOS") 
                                         #+win32 '("win")
                                         #+linux '("linux")
                                         )
                      :host (pathname-host libpath) :device (pathname-device libpath)
                      :name "SuperVP" #+win32 :type #+win32 "exe")))

(defmethod default-supervp-params-folder () (get-pref-value :files :tmp-file))

(add-preference-section :externals "om-SuperVP" nil '(:supervp-path :supervp-authorize :supervp-temp-folder))
(add-preference :externals :supervp-path "SuperVP exec" :file 'default-supervp-path)
(add-preference :externals :supervp-authorize "Authorize" :action 'authorize-supervp)
(add-preference :externals :supervp-temp-folder "Param folder" :folder 'default-supervp-params-folder) 


(defun svp-path () (om::real-exec-pathname (om::get-pref-value :externals :supervp-path)))
(defmethod svp-paramfile ((name string)) (merge-pathnames name (om::get-pref-value :externals :supervp-temp-folder)))
(defmethod svp-paramfile ((name null)) (om::get-pref-value :externals :supervp-temp-folder))


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

(defmethod! authorize-supervp ()
   (forum-authorize (get-pref-value :externals :supervp-path)))

