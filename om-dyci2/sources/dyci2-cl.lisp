
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


(in-package :cl-user)

;;; DYCI2 Lisp package
(defpackage :dyci2
  (:use :common-lisp :cl-user))

;;; define and load the foreign C library
(defun load-dyci2-lib ()
  (let ((libpath (merge-pathnames
                  "lib/mac/libdyci2.so"
                  (om::mypathname (om::find-library "om-dyci2")))))
    (om-fi::om-load-foreign-library
     "LIBDYCI2"
     `((:macosx ,libpath)
       (t (:default "libdyci2"))))
    ))

(load-dyci2-lib)

;;; C functions bindings
(in-package :dyci2)

;;; wrappers to functions exported in the C library
(cffi::defcfun ("Dyci2Init" Dyci2Init) :pointer (pathToDyci2 :string) (filename :string))
(cffi::defcfun ("Dyci2Quit" Dyci2Quit) :int (pyPtr :pointer))

(cffi::defcfun ("Dyci2MakeList" Dyci2MakeList) :pointer (size :int))
(cffi::defcfun ("Dyci2FreeList" Dyci2FreeList) :int (pyPtr :pointer))
(cffi::defcfun ("Dyci2ListAddString" Dyci2ListAddString) :int (pyPtr :pointer) (item :string) (pos :int))

(cffi::defcfun ("Dyci2MakeGenerator" Dyci2MakeGenerator) :pointer (pyPtr :pointer) (size :int) (cSequence :pointer) (cLabels :pointer))
(cffi::defcfun ("Dyci2FreeGenerator" Dyci2FreeGenerator) :int (pyGen :pointer))

(cffi::defcfun ("Dyci2GenQuery" Dyci2GenQuery) :int (pyPtr :pointer) (pyGen :pointer) (size :int) (pyHandle :pointer))
(cffi::defcfun ("Dyci2FreeQuery" Dyci2FreeQuery) :pointer (pyPtr :pointer) (Generator :pointer) (length :int))

(cffi::defcfun ("Dyci2GenOutputSize" Dyci2GenOutputSize) :int (pyGen :pointer))
(cffi::defcfun ("Dyci2GenNthOutput" Dyci2GenNthOutput) :string (pyGen :pointer) (n :int))
(cffi::defcfun ("Dyci2ParametersMod" Dyci2ParametersMod) :int (Generator :pointer))
(cffi::defcfun ("Dyci2SetParametersINT" Dyci2SetParametersINT) :pointer (Generator :pointer) (parameter :string) (value :int))


(push :dyci2 *features*)

;;; for debug:
;(listen *terminal-io*)

(defvar *dyci2-dict* nil)

(defun init-dyci2-lib (path)
  (if (and path (probe-file path))
      (progn
        (princ (format nil "Loading DYCI2 from ~s~&" path))
        (setf *dyci2-dict* (Dyci2Init (namestring path) "load"))
        )
    (print "Warning: DYCI2 folder not found!"))
  (if (cffi-sys::null-pointer-p *dyci2-dict*)
      (print "Warning: DYCI2 lib not initialized!"))
  *dyci2-dict*)


;;; returns a pointer to a DYCI2 genetrator
(defun dyci2-make-generator (size seq lbls)

  (if *dyci2-dict*

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
    (progn
      (print "The DYCI2 library is not initialized !")
      nil)
    ))


;;; queries a DYCI2 generator
(defun dyci2-query (gen query)

  ;;; make query
  (let* ((size (length query))
         (query-ptr (Dyci2MakeList size)))

    (loop for i from 0 to (- size 1)
          for query-elt in query do
          (Dyci2ListAddString query-ptr query-elt i))

    (if (= -1 (Dyci2GenQuery *dyci2-dict* gen size query-ptr))

        (print "[!!] Error generating query !!")

      ;;; query succeeded
      (let* ((outputsize (Dyci2GenOutputSize gen))
             (output (loop for i from 0 to (- outputsize 1) collect
                           (Dyci2GenNthOutput gen i))))

        ;;; free query
        (Dyci2FreeList query-ptr)
        output))))



#|
;;; TESTS

(listen *terminal-io*)

(setq gen (dyci2-make-generator 4 '("(0 5333)" "(5333 10667)" "(10667 13333)" "(13333 16000)") '("A" "B" "C" "D")))

(dyci2-query gen '("D" "A" "B"))

|#
