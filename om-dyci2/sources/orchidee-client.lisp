(in-package :om)


(defvar *orchidee-app-process* nil)
(defvar *listen-osc* nil)
(defvar *last-msg* nil)


(defmethod! set-orchidee-path ((path string))
  :icon '(186)
  (setf *orchidee-PATH* (pathname path)))

(defmethod! set-orchidee-path ((path pathname))
  :icon '(186)
  (setf *orchidee-PATH* path))

(defmethod! set-orchidee-path ((path null))
  :icon '(186)
  (setf *orchidee-PATH* (om-choose-file-dialog)))

;(defmethod! set-orchidee-db-path ((path string))
;  :icon '(186)
;  (setf *orchidee-db-PATH* (pathname path)))

;(defmethod! set-orchidee-db-path ((path pathname))
;  :icon '(186)
;  (setf *orchidee-db-PATH* path))

;(defmethod! set-orchidee-db-path ((path null))
;  :icon '(186)
;  (setf *orchidee-db-PATH* (om-choose-directory-dialog)))


(defun init-orchidee () nil)
(init-orchidee)    

; (setf *current-lib* (exist-lib-p "OM-Orchidee"))

; (orchidee-msg 'progress-bar "/ready" "/isready")

(defmethod! check-orchidee ()
  :icon 899 
  (orchidee-msg 5 "/ready" "/isready"))

; (orchidee-msg 5 "/dbfieldvaluelist" "/dbgetfieldvaluelist" 321 "message" "instrument")

(defun clear-orchidee ()
  (when (and *orchidee-app-process* (orchidee-msg 5 "/acknowledge" "/closesession" 0))
    (orchidee-msg 'progress-bar "/ready" "/dbreset" 1)
    (orchidee-msg 5 "/acknowledge" "/newsession" 0)))

(defmethod! reset-orchidee ()
    :icon 899
    (clear-orchidee)
    (set-db-path *orchidee-db-PATH*)
    )

(defmethod! orchidee-version ()
    :icon 899
    (let ((rep (orchidee-msg 5 "/version" "/version")))
      (when rep (caddr rep))))


(defmethod! run-orchidee ()
  :icon 899
  (if *orchidee-app-process*    
      (progn
        (om-print "Orchidee is already running")
        (when (orchidee-msg 5 "/ready" "/isready")
          (print "ORCHIDEE SERVER OK")))
    (let ((path *orchidee-path*))
      (if (or (null path) (not (probe-file path)))
          (om-message-dialog "Orchidee application not found. Please run it manually or set the application path in OM preferences.")
        (progn 
          (setf *orchidee-app-process* (om-run-application *orchidee-path*))
          (when (orchidee-msg 30 "/ready" "/isready")
            (print "ORCHIDEE SERVER OK")
            (set-db-path *orchidee-db-PATH*))
          ))))
  )


(defmethod! quit-orchidee ()
    :icon 899
    (when (orchidee-msg 5 "/quit" "/quit")
      (setf *orchidee-app-process* nil)))

(defun quit-orchidee-before-exit ()
    (when (and *orchidee-app-process* (om-y-or-n-dialog (format nil "Orchidee is still running.~%Close server before quit ?"))
      (quit-orchidee))))


(om-add-exit-cleanup-func 'quit-orchidee-before-exit)


(defmacro orchidee-handle-server-error (&body body)
  `(handler-bind ((error #'(lambda (e) 
                            (setq err e)
                            (cond ((string-equal (format nil "~A" err) "cannot bind local address/port")
                                   (om-message-dialog (format nil "OM cannot bind OSC port number ~D.~%~%Another request may still be pending, or another Orchidée client (e.g. Orchis) is active.~%~%Only one Orchidée client can be connected at a time." *orchidee-IN*)))
                                  (t (om-message-dialog (format nil "~A" e))))
                            (om-abort))))
    
    ,@body))


; (show-message-win "Orchidee is working. Please wait...")

(defun orchidee-msg (wait repmess &rest data)
  (let ((smsg data))
    (when *print-osc-msg* (print (format nil "SEND TO ORCHIDEE: ~A" smsg))) 
    (if wait
        (let* ((reply nil)
               (message nil)
               (wait-time (if (numberp wait) wait 10))
               (count 0)
               ;(*message-win* (init-message-win))
               (answer-process (orchidee-handle-server-error 
                                (om-start-osc-server *orchidee-IN* "localhost"  
                                                   #'(lambda (msg)
                                                       (let* ((rmsg (om-decode-msg-or-bundle msg))
                                                             (msgcontents (if (listp (cadr rmsg)) (cadr rmsg) rmsg)))
                                                         (when *print-osc-msg* (print (format nil "RECEIVED FROM ORCHIDEE: ~A" msgcontents)))
                                                         (when (and (equal wait 'progress-bar)
                                                                    (string-equal (car msgcontents) "/busy")
                                                                    (> (length msgcontents) 1))
                                                           (setf count 0)  ;; reinit time-out
                                                           (update-progress-bar (cdr msgcontents))
                                                           
                                                           )
                                                         (when (and (not reply)
                                                                    (or (not repmess) 
                                                                        (and (stringp (car msgcontents))
                                                                             (or (string-equal repmess (car msgcontents))
                                                                                 (string-equal "/error" (car msgcontents))))))
                                                           (setf reply t)
                                                           (setf message msgcontents)
                                                           )
                                                         nil)))))
               )
          ; (when (equal wait 'progress-bar)
          ;   (init-progress-bar))
          (om-send-osc-message *orchidee-OUT* *orchidee-HOST* smsg)
          (when (equal wait 'progress-bar)
            (show-message-win "Orchidee is working. Please wait..." :size (om-make-point 400 110)))
          
          (loop ; for count = 0 then (+ count 1)
                while (not reply) do
                (sleep 0.2)
                (setf count (+ count 1))
                (when (> count (/ wait-time 0.2))
                  (if (om-y-or-n-dialog (string+ "Server not responding to " (car smsg) ". Do you want to keep waiting ?"))
                      (setf count 0)
                    (setf reply t))))
   
          (om-stop-osc-server answer-process)
          (when (string-equal "/error" (car message))
            (om-message-dialog (format nil "ERROR at ~A ~A.~%=> ~s" (car smsg) (cadr smsg) (print (cadr message)))))
          (when (equal wait 'progress-bar)
            (hide-message-win))
          message)
    (oa::om-send-osc-message *orchidee-OUT* *orchidee-HOST* smsg))
    ))


(defun update-progress-bar (msgs)
  (change-message-win (format nil "~A ~%~D %" 
                                     (or (nth 1 msgs) "...") 
                                     (round (* 100 (nth 0 msgs))))))



#|

(defun orchidee-msg2 (wait repmess &rest data)
  (let ((smsg data))
    (when *print-osc-msg* (print (format nil "SEND TO ORCHIDEE: ~A" smsg))) 
    (if wait
        (let* ((reply nil)
               (message nil)
               (count 0)
               (wait-time (if (numberp wait) wait 10))) 
          
          
          (orchidee-listen)

          
          (om-send-osc-message *orchidee-OUT* *orchidee-HOST* smsg)
          
          (when (equal wait 'progress-bar)
            (init-progress-bar-process)
            (show-message-win "Orchidee is working. Please wait..." :size (om-make-point 400 110))
            (progress-bar-listen))
                  
          (setf message (orchidee-reply-message (car smsg) repmess wait-time (equal wait 'progress-bar)))
    
          
          (when (equal 'abort message)
            (print "Wait loop aborted")
            (setf message nil))

          (when (string-equal "/error" (car message))
            (om-message-dialog (format nil "ERROR at ~A ~A.~%=> ~s" (car smsg) (cadr smsg) (print (cadr message)))))
          

          
          (when (equal wait 'progress-bar)
            (kill-progress-bar-process)
            (hide-message-win))
          
          message)
    
      (oa::om-send-osc-message *orchidee-OUT* *orchidee-HOST* smsg))
    
    ))


(defvar *orchidee-listen-process* nil)
(defvar *orchidee-incoming-messages* nil)
(defvar *orchidee-progressbar-messages* nil)
(defvar *orchidee-replies* nil)
(defparameter *orchidee-listen-enable* t)

(defun orchidee-listen ()
  (setf *orchidee-incoming-messages* nil)
  (unless (and *orchidee-listen-process*
               (mp:process-alive-p *orchidee-listen-process*))
    (setq *orchidee-listen-process* 
          (om-start-osc-server *orchidee-IN* "localhost" 
                               #'(lambda (msg host) 
                                   (let* ((rmsg (om-decode-msg-or-bundle msg))
                                          (msgcontents (if (listp (cadr rmsg)) (cadr rmsg) rmsg)))
                                     (when *print-osc-msg* (print (format nil "RECEIVED FROM ORCHIDEE: ~A" msgcontents)))
                                     (when *orchidee-listen-enable*
                                       (if (and (string-equal (car msgcontents) "/busy")
                                                (> (length msgcontents) 1))
                                           (pushr msgcontents *orchidee-progressbar-messages*)
                                         (pushr msgcontents *orchidee-incoming-messages*)))
                                     
                                     nil))))))

; (mp::process-kill *orchidee-listen-process*)

(defun my-work-function ()
 (mp:ensure-process-mailbox)
 ;; This should really have an error handler.
 (loop (let ((event (mp:process-read-event (mp:process-event-queue
                                              (mp:get-current-process))
                                             "waiting for events")))
           
           (cond ((consp event)
                  (apply (car event) (cdr event)))
                 ((functionp event)
                  (funcall event))))))




(defvar *wait-for-reply-process* nil)

(defun orchidee-reply-message (sent expected-rep timeout &optional progbar)
  (let ((rep nil))
    ;(init-reply-process)
    (orc-reply-listen sent expected-rep timeout progbar)
    ;rep
    ))

(defun init-reply-process ()
  (unless (and *wait-for-reply-process*
               (mp:process-alive-p *wait-for-reply-process*))
    (setq *wait-for-reply-process*
          (mp:process-run-function "Reply Messages process" () 'my-work-function))))


(defun orc-reply-listen (sent expected-rep timeout progbar)
  (let ((rep nil)
        (count 0)
        (deltaloop 0.1)
        (curmsg nil))
    (loop while (not rep) do
          (setf curmsg (pop *orchidee-incoming-messages*))
          ;(print curmsg)
          (when (or (not expected-rep) 
                    (and (stringp (car curmsg))
                         (or (string-equal expected-rep (car curmsg))
                             (string-equal "/error" (car curmsg)))))
            (setf rep curmsg)
            (push (list sent curmsg) *orchidee-replies*))
          (unless rep
            (when (and (> count (/ timeout deltaloop)) (not progbar))
              (if (om-y-or-n-dialog (string+ "Server not responding to " sent ". Do you want to keep waiting ?"))
                  (setf count 0)
                (setf rep 'abort)))
            (unless (zerop count) (sleep deltaloop))
            (setf count (+ count 1))
            )
          (sleep 0.1)
          )
    rep))


(defun orc-reply-listen-process (sent expected-rep timeout progbar)
  (mp:ensure-process-mailbox *wait-for-reply-process*)   
  (mp:process-send 
   *wait-for-reply-process*
   #'(lambda ()
       (let ((rep nil)
             (count 0)
             (deltaloop 0.1)
             (curmsg nil))
       (loop while (not rep) do
             (setf curmsg (pop *orchidee-incoming-messages*))
             ;(print curmsg)
             (when (or (not expected-rep) 
                       (and (stringp (car curmsg))
                            (or (string-equal expected-rep (car curmsg))
                                (string-equal "/error" (car curmsg)))))
               (setf rep curmsg)
               (push (list sent curmsg) *orchidee-replies*))
             (unless rep
               (when (and (> count (/ timeout deltaloop)) (not progbar))
                 (if (om-y-or-n-dialog (string+ "Server not responding to " sent ". Do you want to keep waiting ?"))
                     (setf count 0)
                   (setf rep 'abort)))
               (unless (zerop count) (sleep deltaloop))
               (setf count (+ count 1))
               )
             (sleep 0.01))
       )))
  )


;;; progress-bar
;;; code from LW support
(defvar *progress-bar-process* nil)
(defvar *abort-progress-bar* nil)

(defun init-progress-bar-process ()
  (unless (and *progress-bar-process*
               (mp:process-alive-p *progress-bar-process*))
    (setq *progress-bar-process*
          (mp:process-run-function "Progress Bar process" () 'my-work-function))))

(defun kill-progress-bar-process ()
  (mp::process-kill *progress-bar-process*))

(defun progress-bar-listen ()
  (setf *orchidee-progressbar-messages* nil)
  (mp:ensure-process-mailbox *progress-bar-process*)   
  (setf *abort-progress-bar* nil)
  (mp:process-send 
     *progress-bar-process*
      #'(lambda () 
         (let ((reply nil)
               (message nil)
               (curmsg nil))
           
           (loop while (and (not reply) (not *abort-progress-bar*)) do ;for j = 0 then (+ j 1) do
                 (setf curmsg (pop *orchidee-progressbar-messages*))
                 ;(print curmsg)
                 (when curmsg
                   (update-progress-bar (cdr curmsg)))
                 (sleep 0.02)
                 ))))
    )
 

;(push '(/busy 0.23 "Export solution set ...") *orchidee-incoming-messages*)
;(pop *orchidee-incoming-messages*)

(defun get-osc-reply (sent expected-rep timeout &optional progbar)
  (let ((rep nil)
        (count 0)
        (deltaloop 0.2))
    ;(setf *orchidee-replies* nil)
    (mp:ensure-process-mailbox *worker-process*)
    
    (mp:process-send 
     *worker-process*
      #'(lambda () 
         (let ((reply nil)
               (message nil)
               (curmsg nil))
           (setf *abort-worker* nil)
           (loop while (and (not reply) (not *abort-worker*)) do ;for j = 0 then (+ j 1) do
                 (setf curmsg (pop *orchidee-incoming-messages*))
                 (when curmsg
                   (cond ((or (not expected-rep) 
                              (and (stringp (car curmsg))
                                   (or (string-equal expected-rep (car curmsg))
                                       (string-equal "/error" (car curmsg)))))
                          (setf reply t)
                          (setf message curmsg))
                         ((and t ; progbar
                               (string-equal (car curmsg) "/busy")
                               (> (length curmsg) 1))
                          (setf count 0)
                          (update-progress-bar (cdr curmsg)))
                         (t nil))
                   )
                 ;(sleep 0.1)
                 )
           (push (list sent message) *orchidee-replies*)
           )
         ))
   
    ;(setf rep (list sent nil))
    (loop while (not rep) do
          (setf rep (cadr (find sent *orchidee-replies* :test 'string-equal :key 'car)))
          (unless rep
            (when (> count (/ timeout deltaloop))
              (if (om-y-or-n-dialog (string+ "Server not responding to " sent ". Do you want to keep waiting ?"))
                  (setf count 0)
                 (setf rep 'abort)))
            (unless (zerop count) (sleep deltaloop))
            (setf count (+ count 1))
            )
          )
    (setf *abort-worker* t)

    rep
    ))

|#
