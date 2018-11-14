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
; 
; LOADER FILE FOR OM 6
;
;============================================================================


(in-package :om)

(om::set-lib-release 1.1 (find-library "om-xmm"))

(compile&load (merge-pathnames "sources/dyci2-cl" *load-pathname*))
(compile&load (merge-pathnames "sources/dyci2-om" *load-pathname*))
(compile&load (merge-pathnames "sources/om6-preferences" *load-pathname*))

(om::fill-library 
 '((nil nil (dyci2::dyci2generator) (set-dyci2-path dyci2::dyci2query dyci2::dyci2setparam) nil)))

(print 
	"
	==============================
	OM-DYCI2
 	==============================
	")
