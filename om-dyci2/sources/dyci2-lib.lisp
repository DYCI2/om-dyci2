
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

;;; pour debug:
;(listen *terminal-io*)

