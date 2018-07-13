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
; 
; LOADER FILE FOR OM 6
;
;============================================================================


(in-package :om)

(om::set-lib-release 1.0 (find-library "om-xmm"))

(compile&load (merge-pathnames "sources/dyci2-lib" *load-pathname*))
(compile&load (merge-pathnames "sources/dyci2-om" *load-pathname*))

(om::fill-library 
 '((nil nil nil (dyci2::dyci2-test) nil)))

(print 
	"
	==============================
	OM-DYCI2
 	==============================
	")
