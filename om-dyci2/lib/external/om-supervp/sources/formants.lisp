;============================================================================
; OM-SuperVP
; SuperVP sound analysis and processing for o7
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
; SPECTRAL ENVELOPPE TOOLS
; File author: J. Bresson (IRCAM - 2010)
;============================================================================


(in-package :svp)

(defmethod! om::make-spec-env ((formants list) &optional (res 1024) (maxf 8000) (db t))
            :icon 530
            :initvals '(((800 0 80)) 1024 8000 t)
            :menuins '((3 (("dB" t) ("Lin." nil))))
            :indoc '("list of formant values" "resolution" "max frequency [Hz]" "amplitude units")
            :doc "Builds a spectral envelope from a list of formants.

The spectral envelope is returned as a BPF object.

Formant values must be formatted as '((frequency1 amplitude1 bandwidth1) (frequency2 amplitude2 bandwidth2) ...) 
Amplitudes are expected in dB.

<res> is the number of points in the envelope.
<maxf> is the value in Hz of the maximum frequency in the envelope.

If <db> is selected (T) amplitudes are returned in dB, else they are returned as linear values.

This function is inspired from initial works by Ph. Depalle and X. Rodet.
"

            (bpf-from-formants formants res maxf db))

(defun bpf-from-formants (formantfilter resolution &optional (maxfreq 10000) (db t))
  (let ((envelope (make-list resolution :initial-element 0))
        (freqs nil))
    (loop for fof in formantfilter do 
          (let ((res (one-formant (car fof) (if (<= (cadr fof) 0) (om::db->lin (cadr fof)) (cadr fof))
                              (caddr fof) 0.001 resolution maxfreq)))
            (setf envelope (om::om+ envelope (cadr res)))
            (unless freqs (setf freqs (car res)))
            ))
    ;(simple-bpf-from-list freqs (om-scale envelope -100 0 0 1) 'bpf 3)
    ;(simple-bpf-from-list freqs (if db (lin->db envelope) envelope)'bpf 3)
    (make-instance 'om::bpf :x-points freqs :y-points (if db (om::lin->db envelope) envelope) :decimals 4)
    ))

(defun one-formant (freq ampl band tex resolution maxfreq)
  (let* ((fof (make-list resolution))
         (fvector (make-list resolution))
         
         (alpha (* PI band))
         (beta (/ PI tex))
         (alpha2 (* alpha alpha))
         (beta2 (* beta beta))
         (cahun (/ PI beta))
         (M (exp (* -1.0 alpha cahun)))
         
         (omega 0)
         (omega2 (* omega omega))
         (module (+ alpha2 omega2))
         (num (+ 1.0 (* M M) (* 2.0 M (cos (* omega cahun)))))
         (den (+ (* module module) (* beta2 (+ beta2 (* 2.0 (- alpha2 omega2))))))
         (max-fof (* (/ beta2 2.0) (sqrt (/ num (* den module))))))
    
    (loop for i = 0 then (+ i 1) while (< i resolution) do
          (let ((f (round (* (/ i resolution) maxfreq))))
            (setf (nth i fvector) f)
            (setf omega (* 2 PI (- f freq)))
            (setf omega2 (* omega omega))
            (setf module (+ alpha2 omega2))
            (setf num (+ 1.0 (* M M) (* 2.0 M (cos (* omega cahun)))))
            (setf den (+ (* module module) (* beta2 (+ beta2 (* 2.0 (- alpha2 omega2))))))
            (setf (nth i fof)
                  (/ (* ampl (/ beta2 2.0) (sqrt (/ num (* den module))))
                     max-fof))
            ))

    (list fvector fof)))



; (FOF 5000 440 -0.25 120 0.001)


#|
CODE MATLAB

 % Retourne la valeur du module de la TF */
 % d'une fof a la pulsation omega        */
 % Phd Le 14 Novembre                    */
 % ROD Le 28 Juin 01                    */
 %  s =  fof_value([-2500:1:2500], 500, 1, 80 , .001) ;
 % axis ( [0 5000 -60 0]) ;
 % plot([0:1:5000],20*log10(s),'k') ;

function [fof_val] = fof_value(LesFreqs, freq, ampl, band, tex)
tex
alpha = pi * band;
beta  = pi / tex;
alpha_car =  alpha * alpha;
beta_car  = beta * beta;
cahun     = pi / beta;
M         = exp( -1.0 * alpha * cahun);

% en zero
omega = 0 ;
omega_car =  omega .* omega;
module    = alpha_car + omega_car;
num = 1.0 + (M * M) + (2.0 * M * cos(omega * cahun));
den = (module .* module) + (beta_car * (beta_car + 2.0 * (alpha_car - omega_car)));
max_fof = (beta_car / 2.0) * sqrt ( num ./ (den .* module));


omega =  2.0 * pi * (LesFreqs - freq) ;
omega_car =  omega .* omega;
module    = alpha_car + omega_car;

num = 1.0 + (M * M) + (2.0 * M * cos(omega * cahun));
den = (module .* module) + (beta_car * (beta_car + 2.0 * (alpha_car - omega_car)));
fof_val = ampl * (beta_car / 2.0) * sqrt ( num ./ (den .* module)) / max_fof;
size(fof_val)

% EXEMPLE

cutFreq = 5000 ;

x = [0:10:cutFreq] ;

f0 = 200 ;
f1 = 500 ;
f2 = 1500 ;
f3 = 2500 ;
f4 = 3500 ;
a0 = .25 ;
a1 = 1 ;
a2 = .25 ;
a3 = .125 ;
a4 = .0625 ;
b0 = 500 ;
b1 = 50 ;
b2 = 50 ;
b3 = 80 ;
b4 = 120 ;

 s =     fof_value(x, f0, a0, b0 , .0007) ;
 s = s + fof_value(x, f1, a1, b1 , .0007) ;
 s = s + fof_value(x, f2, a2, b2, .0007) ;
 s = s + fof_value(x, f3, a3, b3, .0007) ;
 s = s + fof_value(x, f4, a4, b4, .0007) ;

 plot(x, 20*log10(s),'k') ;

 axis ( [0 cutFreq -60 0]) ;

|#

#|

CODE LELISP

(setq %pi 1.314159)
 (setq %samplingFreq 1.314159)

(de trace_fof (reso ampl band tex)
  (let ( (spect_fof (makevector 256 0.)) )
     (setq alpha (fmul %pi band))
     (setq beta (fdiv %pi tex))
     (setq omega (fsub 0. (fmul 2. (fmul %pi reso))))
     (setq pas_puls (fmul %samplingFreq (fmul 2. (fdiv %pi 256.))))
     (for (i 0 1 255)
          (vset spect_fof i
                          (fmul -10. (log
                          (fmul ampl (fof_transfert alpha
                                         beta
                                         (setq omega (fadd pas_puls omega))))))))
     spect_fof))

(de fof_transfert (alpha beta omega)
   (setq cahun (fdiv %pi beta)
         beta_car (fmul beta beta)
         omega_car (fmul omega omega)
         alpha_car (fmul alpha alpha)
         mode (fadd alpha_car omega_car)
         m (exp (fsub 0. (fmul alpha cahun))))
    (setq square_num (sqrt
           (fadd (fadd (fmul m m) 1.) (fmul 2. (fmul m (cos (fmul omega cahun)))))))
    (setq deno2 (sqrt (fadd (fmul mode mode)
             (fmul beta_car
                   (fadd (fadd beta_car (fmul 2. alpha_car))
                         (fmul -2. omega_car))))))
    (setq transfert (fdiv (fmul square_num beta_car)
            (fmul 2.0 (fmul deno2 (sqrt mode))))))

(de fof (reso ampl band tex min max)
 (let ( (spect_fof (trace_fof reso ampl band tex)) )
   (initshowt4 'p -2. 129. min max)
   (for (i 0 1 127) (dr moi (float i) (vref spect_fof i))))))
|#

