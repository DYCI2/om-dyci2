
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
; Authors: Victoire Siguret, Jean Bresson, Jerome Nika - IRCAM/STMS 2018
;============================================================================

(in-package :dyci2)

;;;========================
;;; OM INTERFACE
;;;========================

(om::defclass! dyci2Generator (om-api:om-cleanup-mixin) 
  ((pyGen :accessor pyGen :initform nil) ;;; pointer sur pyGenerator
   (memory :accessor memory :initform nil :initarg :memory)   
   (labls :accessor labls :initform nil :initarg :labls)))

;;; automatically called by the garbage-collector
(defmethod om-api::om-cleanup ((self dyci2Generator))
  (when (pyGen self)
    (om::om-print (format nil "Free DYCI2 Generator: ~A" (list (pyGen self))))
    (if (cffi-sys::null-pointer-p (pyGen self))
        (print "Warning: DYCI2 Generator was allready a NULL pointer!")
      (dyci2freegenerator (pyGen self)))
    (setf (pyGen self) nil)))


#+om7
(defmethod om::om-init-instance ((self dyci2Generator) &optional args)
  (om-api::om-cleanup self) ;; just in case...
  (setf (pyGen self) (dyci2-make-generator (length (memory self)) (memory self) (labls self)))
  self)
  

#+om
(defmethod om::make-one-instance ((self dyci2Generator) &rest slots-vals)
  (om-api::om-cleanup self) ;; just in case...
  (let ((rep (call-next-method)))
    (setf (pyGen rep) (dyci2-make-generator (length (memory rep)) (memory rep) (labls rep)))
    rep))
 

(om::defmethod! dyci2query ((self dyci2Generator) (query list))
  (dyci2-query (pyGen self) query))


(om::defmethod! dyci2setparam ((self dyci2Generator) (parameter string) (value integer)) 
  ;; (Dyci2ParametersMod (pygen self))
  (if (pygen self)
      (Dyci2SetParametersINT (pygen self) parameter value)
    (om::om-beep-msg "DYCI2 Generator not initialized !!"))
  self)



;;; TODO
#|
(om::defmethod! dyci2queryfree ((self dyci2Generator) size)

   (Dyci2GenQueryFreePy *dyci2-dict* self size)

  (let* ((outputsize (print (Dyci2GenOutputSize (pygen self))))
         
         (output (loop for i from 0 to (- outputsize 1) collect
                       (Dyci2GenNthOutput (pyGen self) i))))

    ;;; free query
    ;;; ...

    output)
)
|#



;;; TEST
; (dyci2-make-generator 11 '("A1" "B1" "B2" "C1" "A2" "B3" "C2" "D1" "A3" "B4" "C3") '("A" "B" "B" "C" "A" "B" "C" "D" "A" "B" "C"))
; (Dyci2GenQuery *dyci2-dict* (pyGen self) query (length query))




























































