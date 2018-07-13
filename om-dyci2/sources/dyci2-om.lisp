
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


(in-package :dyci2)


(defvar *dyci2-dict* nil)

(defun init-dyci2-lib (path)
  (setf *dyci2-dict* (Dyci2Init (namestring path) "load")))


;;;========================
;;; MAIN CLASS
;;;========================

(om::defclass! dyci2Generator (om-api:om-cleanup-mixin) 
  ((pyGen :accessor pyGen :initform nil) ;;; pointer sur pyGenerator
   (memory :accessor memory :initform nil :initarg :memory)   
   (labls :accessor labls :initform nil :initarg :labls)))

;;; automatically called by the garbage-collector
(defmethod om-api::om-cleanup ((self dyci2Generator))
  (when (pyGen self)
     (om-lisp:om-print-format "Free DYCI2 Generator: ~A" (list (pyGen self)))
     (dyci2freegenerator (pyGen self))
     (setf (pyGen self) nil)))

(defmethod om::om-init-instance ((self dyci2Generator) &optional args)
  
  (om-api::om-cleanup self) ;; just in case...
  (setf (pyGen self) (dyci2-make-generator (length (memory self)) (memory self) (labls self)))
  self)
  

; (listen *terminal-io*)

(defmethod dyci2-make-generator ((size integer) (seq list) (lbls list))
  (if *dyci2-dict*
      ;;; then
      (let* ((seq-ptr (Dyci2MakeList size))
             (labels-ptr (Dyci2MakeList size)))
        
        (unwind-protect 
            ;;; "protected" execution
            (progn 
              (loop for i from 0 to (- size 1) 
                    for seq-elt in seq 
                    for lab-elt in lbls
                    do
                    (Dyci2ListAddString seq-ptr seq-elt i)
                    (Dyci2ListAddString labels-ptr lab-elt i))
        
              (Dyci2MakeGenerator *dyci2-dict* size seq-ptr labels-ptr))
        
          ;;; cleanup
          (Dyci2FreeList seq-ptr)
          (Dyci2FreeList labels-ptr)
          ))
        
    ;;; else
    (om::om-beep-msg "DYCI2 NOT INITIALIZED!!")
    )
  )


(om::defmethod! dyci2setparam ((self dyci2Generator) (parameter string) (value integer))
  
  ;; (Dyci2ParametersMod (pygen self))
        
  (if (pygen self)
        
         (Dyci2SetParametersINT (pygen self) parameter value)
     
     (om::om-beep-msg "GENERATOR NOT INITIALIZED!!"))
  
  self)



(om::defmethod! dyci2query ((self dyci2Generator) (query list))

  ;;; make query
  (let* ((size (length query))
         (query-ptr (Dyci2MakeList size)))
  
    (loop for i from 0 to (- size 1) 
          for query-elt in query do
          (Dyci2ListAddString query-ptr query-elt i))
    
    (if (= -1 (Dyci2GenQuery *dyci2-dict* (pyGen self) size query-ptr))
        
        (print "ERROR GENERATING QUERY...")
  
      ;;; query succeeded
      (let* ((outputsize (Dyci2GenOutputSize (pyGen self)))
             (output (loop for i from 0 to (- outputsize 1) collect
                           (Dyci2GenNthOutput (pyGen self) i))))
        
        ;;; free query
        (Dyci2FreeList query-ptr)
        
        output))))


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




























































