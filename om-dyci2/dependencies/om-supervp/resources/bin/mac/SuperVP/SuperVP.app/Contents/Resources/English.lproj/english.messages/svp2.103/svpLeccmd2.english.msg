leccmd:usage-cross-ps =

-cross_ps  <value> ::                   %s                  (Def: 0.3)

   Controls the phase synchronization  scale for the generalized cross
   synthesis  mode (see %s).   When doing  cross synthesis  in -Gcross
   mode the  phase synchronized bins  that have been connected  to the
   central bin  (see: -P  and -ps_fac) will  be disconnected  from the
   center bin  according to the relative  frequency mixing coefficient
   Y/(Y+y) and y/(Y+y).  If phase synchronization is enabled the phase
   synchronization  information will  always be  taken from  the track
   that   has  the  stronger   frequency  mixing   coefficient.  Phase
   synchronization  will  be disabled  when  the  track with  stronger
   frequency mixing coefficient  has relative mixing coefficient below
   1-<value>.  If the  relative frequency  contribution is  above this
   threshold the synchronized bins  will be disconnected as a function
   of  their  frequency distance  to  the  master  (=center) bin.   If
   transient synchronization is enabled a similar treatment is applied
   to the phase synchronization due to transient events. The effect is
   that the more  the mixing coefficient decreases the  less the phase
   synchronization will take  place.  Increasing <value> increases the
   effect  of  phase   synchronization  for  smaller  relative  mixing
   factors.
   
   <values> range is in [0, 0.5].

----------------------------------------------------------------------~

leccmd:usage-cross-exp =

-cross_exp  <file or value> ::         %s                   (Def: 0.5)

    selects  the exponent  of the  amplitude product  that is  used to
    achieve  nonlinear amplitude modulation  in the  generalized cross
    synthesis mode  -Gcross (see %s ).   The argument can  either be a
    constant value  or an break point function.   Exponent values have
    to be positive or 0. The default value is 0.5 which will result in
    using  the  sqrt  of   the  amplitude  product  as  the  nonlinear
    contribution to the output amplitude.

    ------ Example:    -cross_exp 1
    
    Sets  the expoentn  to one  such that  the amplitude  product used
    directly as it was the case in all historical version of SuperVP.

----------------------------------------------------------------------~

leccmd:usage-F =
-F0/-f0 <file or value> :: %s

   Specifies  an  f0 analysis  file  (ascii bpf  or  SDIF  1FQ0) or  a
   constant value that will be used  as f0 value for the current input
   file.  The values  are  mainly used  in  conjunction with  envelope
   estimation/preservation see (-Afft ord/-Atenv ord/-envtr/-envf0sc)

   ------ Example:    -F0 200    

         use a fixed value for the f0 of the whole file

   ------ Example:  -F0 f0.sdif  

        read the f0 values from the sdif file f0.sdif

----------------------------------------------------------------------~

leccmd:usage-filterintro =   
======================================================================
=========================== Filtering%s ===============================
   
SuperVP comes with a large collection of spectral domain filters.
The available filters are 

%s
  bande      : freely configurable multi band stop and pass bands  %s
  gabarit    : regularly sampled transfer functions (gabarit or SDIF)  
               amplitude, phase or real, complex   %s
  envscal    : apply scaled envelopes in SDIF, gabarit or cepstral format  %s
  breakpt    : ASCII break point function   %s
  surface    : constant/ramped attenuation of jordan surfaces in the time frequency plane   %s
  fof,fifof  : formant filters  %s
  clip       : amplitude dependend signal attenuation  %s
  fshift     : frequency shift (ring modulation)  %s 
  sub        : spectral subtraction (new denoise)  %s
  denois     : surface filter with amplitude depend attenuation (old denoise)  %s
  vconv      : voice conversion using GMMs %s
%s

The general format of the filter modules is: 

-Ff<type> <filename> ::  
   
   the type specifies the filter type which is one of the types in the
   list  above.   filename is  a  file  containing filter  parameters.
   Filters specified with  "F" are applied to the  signal in the first
   track. Filter specified with "-f"  are applied to the signal in the
   second track.

   Any  combination of  filters  can  be applied  during  one call  to
   SuperVP (see %s) by meansof giving multiple filter options.
   
   for a  description of available  filters and the  related parameter
   file format.

   ------ Example: -Fbande file

----------------------------------
   
-Ff<superposition mode> ::                             (Def: Multiply)
   
   If multiple filters of  the types bande, gabarit, breakpt, surface,
   fof,  fifof  are applied  the  standard  behavior superimposes  the
   filters  by  means of  multiplying  the  transfer functions.   This
   behavior is  explicitely selected by  specifying -FCombineMul.  The
   alternative is  -FCombineMax which will only apply  the filter with
   maximum amplitude.

   The Mode of Superposition may be selected independently for the two
   tracks

   ------ Example: -FCombineMax -fCombineMul

---------------------------------------------------------------------
~

leccmd:usage-ffile =
=====================================================================
------------------ Filter module description%s -----------------------

The following section describes the available filter modules in detail: 

%s
~

leccmd:usage-ffile-bande =
bande,bande-noex,band-transp,band-noex-transp :: %s

   applies  a band pass/band  stop filter  with possibly  time varying
   band edges. The parameter file contains multiple lines of

%s
   Example: time    num  type  f_0  f_1   f_2  ...  f_i 
             0.1     4    1     0   1000  4500 ... 16023

   time : time position for following filter description  
   num  : number of band edge frequencies to follow 
          each pair of frequencies constitutes a band
          so num has to be even. First band is [f_0,f_1]
          seoncd band is [f_2,f_3] ...
   type : type of given bands either 1 (pass bands) or 0 (stop bands)
   fi   : num frequencies f(0),..,f(num-1) that specify the edges of 
          the frequency bands. Bands may self-intersect.
          The lower edge of a band is always min(f(2*i),f(2*i+1))
          and the upper edge is max(f(2*i),f(2*i+1)).
          Included into the band i are all bins with frequency 
          min(f(2*i),f(2*1+1)) <= freq <=  max(f(2*i),f(2*i+1)) 
%s        
   For time varying filters only the values f_i may change with time.

   Extrapolation:

   In normal operation the first  and last lines of the parameter file
   are  extrapolated without changes  to start/end  of sound.   If the
   flag -noex  is appended to the  filter type the filter  will not be
   extrapolated but  only applied inside the time  limits specified in
   the parameter file

   Transparency:	

   If  -transp is  added to  the filter  type the  filter  behavior is
   changed with  respect to the  mode of superposition  CombineMax. In
   this  case pass bands  are considered  transparent with  respect to
   filter superposition  such that they are not  included in selecting
   the maximum value of the transfer function.

--------------------------------------------------------~
 
leccmd:usage-ffile-clip =
clip, clip-norm::  %s

   apply  clipping  filter  within  time  slices of  the  sound.   The
   clipping filter clips all amplitudes  of a spectrum above the upper
   threshold and  rescales the amplitudes between  lower threshold and
   upper threshold to  lie between 0 and upper  threshold.  A clipping
   filter  can be  described  in  two modes. The first  mode describes  a
   clipping filter operating on the whole sound file

   Example: low_threshold high_threshold

   this  will result in  applying clipping  with the  given parameters
   globally for the whole sound or

   Example: low_threshold high_threshold start_time end_time

   this will apply the  respective clipping parameters only within the
   specified  time  region.  Multiple  lines may  be  specified.   For
   overlapping  regions the first  region takes  precedence thresholds
   parameters are  assumed to  be in dB  with respect to  a normalized
   amplitude spectrum (max. = 0dB)

   Normalization:
   
   if  the special  type clip-norm  is specified  the  clipping filter
   rescales the resulting signal to preserve the signal energy.

--------------------------------------------------------~

leccmd:usage-ffile-gabarit =
gabarit,gabarit-noex::  %s

   apply a  fft analysis output file  that has been  stored in gabarit
   format as a filter to the input sound file.

   The gabarit mode has to be  1 or 6 (amplitude only), 2 (phase only)
   or 3 for amplitude and  phase filtering (See option -Og).  The data
   can  be stored  in the  SuperVP binary  gabarit format  or  in SDIF
   format.  Since SuperVP 1.83  the filter analysis data sampling rate
   and fftsize are  no longer required to match  the respective values
   for  the filtered  sound.  Any  mismatch will  result  in a  proper
   resampling of  the filter data using linear  interpolation of phase
   and amplitude (amplitude interpolated  in log domain) such that the
   frequency response is kept.

   Gabarit format description:

   All the different formats share a common file header specifying

%s
       gabarit mode (4 byte int)
       sample rate (4 byte float)
       soundfilenamelen (4 byte int)
       soundfilenamechars (namelen chars)
%s

   The length of the input sound file is always including the limiting
   zero byte of a c-string and rounded up to regard a 4 byte boundary,
   the unused part of the name is filled with zeros.

   Following the file header are  the frames each with a unified frame
   header consisting of

%s
      frameCenterTime (4 byte float)
      nbdata          (4 byte int)
      windowSize      (4 byte int)
      frameSize       (4 byte int)
%s

   The samplerate is  used to derive the data  spacing in frequency as
   samplerate/(nbdata-1)/2.  Windowsize is  used to  adjust  the phase
   prior to  using it by  adding/subtracting a linear  phase component
   that  compensates  for  the  center  of the  window  being  shifted
   (windowsize-1)/2 away from the start of the frame. Framesize is not
   used for filtering  and can be left 0. After  each header there are
   nbdata multi  element values. The  meaning of the values  depend on
   the mode and are: 

%s
      mode  |  data
      ======|=======================
       1    |  single amplitude
       2    |  single phase value
       3    |  pair of amplitude and phase value
       4    |  pair of frequency and amplitude value (not suitable for filtering)
       5    |  pair of real imaginary value (not suitable for filtering)
       6    |  single amplitude
%s

   All data points are in 32 bit float format

   In  the SDIF  version of  the gabarit  format the  header  and data
   information are stored in respective sdif matrices.

   Note that  only modes 1-3  and 6 can  be used as a  gabarit filter.
   The modes 1-3  create a transfer function from  amplitude and phase
   values  which is applied  to the  respective analysis  frames.  The
   mode 6  is different in  that it operates nonlinearly  and replaces
   the amplitude values of the  incoming frame by the amplitude values
   of the filter.
      
   The phase interpolation is  done after phase unwrapping. Therefore,
   the maximal spacing of synthetic phase transfer functions has to be
   selected such that phase differences of the phase transfer function
   between the bins are smaller then PI.

   Filtering using CEC and ARA Filters :

   Since version 2.91.0  the gabarit filter can be  specified in terms
   of cepstral coefficients using the  1CEC sdif frames or by means of
   all pole filter  coefficients and gain factors using  the 1ARA sdif
   frames. In these cases the  coefficients are transformed on the fly
   into into  the respective Fourier transform  representation and the
   transfer functions are interpolated as described above.

   To  enable phase filtering  with the  coefficient filters  the flag
   -envp has to be set on the command line.

   Extrapolation:

   In normal operation the first  and last lines of the parameter file
   are  extrapolated without changes  to start/end  of sound.   If the
   flag -noex  is appended to the  filter type the filter  will not be
   extrapolated but  only applied inside the time  limits specified in
   the parameter file

--------------------------------------------------------~

leccmd:usage-ffile-surface = 
surface ::  %s

   Applies  constant  gain to  a  piece  wise  linear surface  of  the
   spectrogram. The surface filter requires the surface to be not self
   intersecting! In technical terms the surface has to be a piece wise
   linear Jordan curve.

   simple surfaces:
   ================

   To specify a surface filter  the surrounding jordan curve has to be
   specified in terms of a sequence of points that make up the outline
   of the curve.
    
   The general syntax to specify a simple jordan curve is : 
%s
   J numpoints gain_db time_1 freq_1 time_2 freq_2 ... time_n freq_n
%s
   This means  each surface starts with  the letter J  followed by the
   number  of points  that make  up  the surrounding  curve. The  next
   number that follows declares the  amplitude gain of the surface and
   the remaining  numpoints pairs  are the time  frequency coordinates
   describing the outline of the surface. Note that, these points have
   to  be given in  the order  that they  would have  if you  draw the
   surface with a pencil. The surface is automatically closed by means
   of connecting the last and first point of the jordan curve.

   Example for single surface with  40dB attenuation: 
%s
   J 10 -40. 0 1500 1 1000 2 1000 3 2000 4 2000 5 2500 4 3000 3 3000 2 2000 1 2000
%s
   the filter  represents the  same surface that  is also used  for the
   backwards compatibility example below.

   Layered surfaces:
   =================

   Since version  2.90.11 surfaces may be layered.   a layered surface
   contains  a   sequence  of  jordan  curves  each   with  a  specfic
   attenuation factor. The curves  are combined such that whenever two
   of  the layers overlap,  the one  that has  been sepcified  last is
   winning.  As a further  improvement the  attenuation factor  of the
   first (lowest) layer  of a layered surfaces may  be ramped in.  The
   ramp  width and  orientation is  specified in  terms of  a distance
   metric that  is used  to calculate the  distnce to the  next border
   point of  the surface. The  attenuation is then linearly  ramped in
   such  that the maximum  attenuation is  achieved when  the distance
   (according to the specified metric) is 1.

   Layered surfaces  are indicated by means of  the starting character
   'L'.  Layers  are separated by means  of comma. Each  layer gets as
   specification the  number of points  in the layer,  the attenuation
   factor and the sequence of points. The layer surface syntax is

%s
   L UT UF NP1 ATT1 S1-T1 S1-F1 ... S1-TL S1-FL , 
            NP2 ATT2 S2-T1 S2-F1 ... S2-TL S2-FL , 
            ...
            NPL ATTL SL-T1 SL-F1 ... SL-TL SL-FL 

   L           : indicates a layered surface to follow
   UT          : unit in seconds  for measuring  distance to border in 
                 time direction 
   UF          : unit in  Hz for  measuring distance to border in freq 
                 direction
              ----> 1. layer
   NP1         : number of points for first layer
   ATT1        : attenuation in dB of first layer
   S1-T1 S1-F1 : NP1  pairs  of  time/frequency  coordinates for first 
                 surface
              ----> 2. layer
   ,           : seperate next surface
   NP2         : number of points for second layer
   ....
%s

   An example is as follows: 

%s   
   L 0.3 200 3 -50  1 100 3 100 2 3000, 3 0 1.5 1000 2.5 1000 2 2000
%s     
   indicates a two layer surface with -50dB in the first layer and 0dB
   in  the  second layer  which  effectively  creates  a hole  in  the
   surface.   Ramp units  used in  the first  layer are  0.3s  in time
   direction and  200Hz in frequency  direction. To specify a  ramp in
   time  (or  frequency) direction  only,  the  frequency (time)  unit
   should be set to an extremely large value.

   Ramped surfaces:
   =================

   Since version 2.92.42  the attenuation of a surface  can be ramped.
   The ramped surfaces  a single (not layered) jordan  curves having a
   reference time  position for that the  maximum/targt attenuation is
   achieved.  For all  other values of the surface  the attenuation is
   linearly increasing for each  frequency from the most distant point
   of the  surface at  that frequency.

   Ramped surfaces  are indicated by  means of the  starting character
   'LR'.  The  ramped surface is specified  by means of  a target time
   position   for  the  maximum   attenuation  a   frequency  position
   (currently  unused),  the number  of  points  in  the surface,  the
   attenuation  factor  and  the  sequence  of points.   If  the  time
   reference is  not covered by  the surface the ramping  is disabled.
   The layer surface syntax is

%s
   R TimeRef FreqRef NP ATT S-T1 S-F1 ... S-TL S-FL  

   R           : indicates a ramped surface to follow
   TimeRef     : time position in seconds at which maximum attenuation 
                 is achieved. 
   FreqRef     : currently unsued
   NP          : number of points for the jordan curve to follow
   ATT         : maximum attenuation in dB 
   S1-T1 S1-F1 : NP   pairs  of  time/frequency  coordinates for the
                 surface
%s

   An example is as follows: 

%s   
   R 3 0  3 -50  1 100 3 100 2 3000
%s     
   indicates a ramped surface with -50dB maximum attenuation at time
   3sec.

   backwards compatibility:
   ========================

   The old surface encoding used in SuperVP versions prior to 2.62f is
   still supported. This encoding  does only allow convex polygons and
   has  therefore been replaced.   The description  of the  old format
   follows: 

%s
      start_time num end_time gain_dB

      start_time/end_time:  the minimum and maximum
           time covered by that surface.

      num: the number of lines to follow describing the shape of 
           the surface.

      gain_dB: The fixed attenuation/amplification for this surface.
%s

   surface description is given by num lines of format
%s
      time low_freq high_freq

      time: specifies the time the description applies
      low_freq/high_freq: specify the frequency boundaries 
          (end points belonging to the surface) for the given time.
%s

   The description  specifies a piece  wise linear bounded  surface in
   the time/frequency plane.  The start_time/end_time locations of the
   surface are centered between  the first and last frequency boundary
   pairs.  Due to this format only convex surfaces can be described.
   
   Example for single surface with  40dB attenuation:
%s
   0.0 4 5.0 -40
   1.0 1000 2000	
   2.0 1000 2000	
   3.0 2000 3000	
   4.0 2000 3000
%s
   Time and frequency values have to be non decreasing.  Superposition
   of different surfaces will respect the selected superposition mode.

--------------------------------------------------------~

leccmd:usage-ffile-breakp =
breakpt,breakpt-noex,breakpt-transp,breakpt-noex-transp::  %s

   These filters  applies a  piece wise linear  description of  a time
   varying or constant frequency response specified in dB/rad.

   The parameter  may contain  an ASCII or  binary description  of the
   filter.  In the  binary  case the  file  has to  start  with the  8
   characters  BPBINARY.   The  parameter  file consists  of  sets  of
   numbers describing the filter as a break point function for a given
   time, as follows

%s
   time mode numpairs freq value freq value

   time(float): time position to apply following filter response definition 

   mode(int): filter mode (0=amplitude/1=phase/2=amplitude and phase)
              mode parameter can not change within a single file

   numpairs(int):  number of freq-value breakpoints to follow in current line

   freq value(float): depending of the selected mode pair or triple of values
               describing a break point of the frequency response

               freq [Hz] and amplitude [dB] for mode = 0
               freq [Hz] and phase [rad] for mode = 1
               freq [Hz], amplitude [dB] and phase [rad] for mode = 2
%s

   The type given  in parenthesis is the data type  of the entries for
   the binary parameter file.

   The  break point description  is extended  over the  full frequency
   range using  a constant  extrapolation of the  start and  end break
   points.   The  envelopes  amplitude   and  phase  values  for  each
   frequency bin  are linearly interpolated over time  and extended to
   the start  and end  of the file  using constant  extrapolation. For
   correct  interpolation   the  phase  values  are   required  to  be
   unwrapped!  For  instantaneous  changes  with respect  to  time  or
   frequency you may give the same frequency or time value twice.

   Example for breakpt file using amplitude mode :

%s
     0.1 0 4 100 0 500 -30 1000 -30 4000 0 
     0.5 0 5 100 0 500 -30 1000 -30 4000 -10 5000 0 
%s

   This file would attenuate the  amplitudes in the region between 100
   and 4000Hz to 100 and 5000Hz respectively.

   Extrapolation:

   In normal operation the first  and last lines of the parameter file
   are  extrapolated without changes  to start/end  of sound.   If the
   flag -noex  is appended to the  filter type the filter  will not be
   extrapolated but  only applied inside the time  limits specified in
   the parameter file

   Transparency:	

   If  -transp is added  to the  filter type  the filter  behavior and
   parameter file is changed with respect to the mode of superposition
   CombineMax. In  this case  the first floating  point number  in the
   parameter file is the transparent  value of the filter with respect
   to filter  superposition. Filter transfer function  values that are
   equal to this value are not included in selecting the maximum value
   of the transfer function.

--------------------------------------------------------~

leccmd:usage-ffile-envscal =
envscal, envscal-noex, envscal-en, envscal-en-noex  %s ......
envscal-ml, envscal-ml-noex  ::   
 
   Envelope scaling filter is derived from a gabarit filter and can be
   used to partly replace a  spectral envelope. The filter argument is
   a gabarit filter  or an AR or CEC envelope  coefficient file with a
   maximum of two scaling  bpf functions added.  The scaling functions
   are attached to the filter  file name separated with by comma.  The
   scaling bpfs that  are not given have as  default value he constant
   1.  The first scaling factor applies to frequency dependent part of
   the filter that  determines the timbre of the  sound, the second to
   the mean level  of the filter.  See %s  for further explanations
   of the mean envelope calculation.

   If M is the mean of the filter and E(w) is the envelope part of the
   filter such that log(A(w)) = log(M) + log(E(w)) and if s(t) and m(t) 
   are the scaling bpfs for envelope and meanl level then the filter
   at time position t that will be applied to the input spectrum is

   F(w,t) = exp((log(E(w))*s(t) + log(M)*m(t))) 
   
   This scaling function corresponds with  the one that is used in the
   -Afft/-Aced modules  to scale  the spectral envelope  and therefore
   the  analysis   modules  and  the  envelope   scaling  filter  work
   coherently together.

   Supported gabarit formats for  the envelope filter are 1 (Amplitude
   only) and 3 (Amplitude and Phase) as well as CEC and ARA envelope
   coefficients

   --- Example :   -Fenvscal  env.sdif,0.5

   applies envelope filter in env.sdif using constant scaling factor
   of 0.5 for the envelope and 1 for the mean envelope.

   --- Example :   -Fenvscal  env.sdif,bpfscal.sdif,0.5

   applies envelope filter in  env.sdif reading the env scaling factor
   from a  bpf file and  using the constant  0.5 for scaling  the mean
   level of the filter.

   Extrapolation:

   In normal operation the first  and last lines of the parameter file
   are  extrapolated without changes  to start/end  of sound.   If the
   flag -noex  is appended to the  filter type the filter  will not be
   extrapolated but  only applied inside the time  limits specified in
   the parameter file

--------------------------------~

leccmd:usage-ffile-formant =
fof fof-noex fifof fifof-noex::  %s
   
   filtering applies a collection of second order resonance or formant
   filters to the sound signal.

   The file consists of lines containing

%s
   time numfof freq gain band ...

   time:    start time for the following parameters
   numfof:  number of formats to follow
%s
   then numfof format descriptions each specifying

%s
   freq:   fof resonance freq in Hz
   gain:   amplitude gain at resonance freq in dB
   bandwidth: formant bandwidth in Hz
%s

   In case  of fof the  number of formant  filters has to be  the same
   throughout  the  file  and  the  parameters for  each  formant  are
   linearly interpolated.
   
   In  case   of  fifof  the   number  of  formants  can   change  and
   interpolation with respect to time is achieved by interpolating the
   resulting transfer functions.

   Example:    fifof.par
   
%s
   0.1 1 440 0 50
   0.2 2 440 0 50 880 -2 100
   0.3 3 440 0 50 880 -2 100 1760 -4 200
%s

   Extrapolation:

   In normal operation the first  and last lines of the parameter file
   are  extrapolated without changes  to start/end  of sound.   If the
   flag -noex  is appended to the  filter type the filter  will not be
   extrapolated but  only applied inside the time  limits specified in
   the parameter file

----------------------------------------~
leccmd:usage-ffile-fshift =

fshift::   %s
   
   this filter applies a time varying frequency shift to the signal

   the parameter  file contains a breakpoint description  of the shift
   to  apply at  a  given time  point  in Hertz.  The  first and  last
   parameter are extended till the start/end of the sound respectively

   Example:
%s
   0.1 -50.
%s
   this file would shift the sound down by 50Hz.

   NOTE:  due to  the internal  representation you  may not  combine a
   frequency shift filter with time stretching or transposition.
    
---------------------------------------~

leccmd:usage-ffile-denois =
   
denoise::  %s
   
   clipping all  amplitudes inside a  time frequency surface  that are
   below the specified clipping value to zero.

   The parameter  file is  equivalent to the  surface filter  with the
   gain parameter  specifying a clipping  level in dB relative  to the
   maximum of a normalized spectrum (0dB).
   
----------------------------------------~
leccmd:usage-ffile-vconv =
    
vconv::   %s
    
    Basically, this filter allows to transform a given voice identity
    to an another given a conversion model.
    Using Gaussian Mixture Model (GMM) for the conversion model, the
    Line Spectral Frequencies (LSF) representation of the
    true-envelope of each frame is converted.

    LSF computation
    ---------------
    To compute the LSF, two methods are available:
    c:  The companion matrix method, which is slow but gives always
        the proper LSF.
        (default method)
    n:  The Newton's method, which is very fast (10 times faster than
        the companion matrix method) but gives noisy LSF for orders
        >40, and can even diverge for orders >60. In case of
        divergence, a message is displayed and the procedure fall
        back to the companion matrix method (see also LSF computation
        help). (note that the above values are approximation only)
    The method can be chosen adding the letter 'c' or 'n' at the end
    of the file name using a comma, as in the example below.

    Gaussian Mixture Models
    -----------------------
    Two GMM are used for the whole conversion. One for the
    "recognition" step and another one for the "transformation" step.
    The recognition GMM allows to estimate the weight of each component
    of the transformation GMM (it recognizes to which class the source
    belongs to). Therefore, the number of component in recognition and
    transformation GMMs are the same. Moreover, the dimension of the
    recognition GMM is usually smaller than that of the transformation
    GMM. Note that the two GMMs can be exactly the same (thus, the LSF
    will computed only once).

    About the dimensions
    --------------------
    The recognition dimension is made of the GAIN AND the LSF (the
    gain is at first position). Conversely, the dimension of the
    transformation is made of the LSF ONLY !

    Conversion model file
    ---------------------
    A model file (ex. the c3po.model2 file below) has to obey to
    the following binary format:
     <modelfile>   ::= <modelversion>, <priors>, nb_comp x <mix_rec>,
                       nb_comp x <mix_trans>
     <priors>      ::= <nb_comp>, nb_comp x <data>
     <mix_rec>     ::= <dim_rec>, <centers>(dim_rec), <covars>(dim_rec)
     <mix_trans>   ::= <dim_trans2>, <centers>(dim_trans2),
                       <covars>(dim_trans2)
     <centers>(dim)::= dim x <data>
     <covars>(dim) ::= dim x dim x <data>
     <nb_comp>     ::= 4 byte signed integer
     <data>        ::= 8 byte float
     <dim_*>       ::= 4 byte signed integer
    (see also the vconv_export_model2.m Matlab function)

    Global LSF normalization
    ------------------------
    The variance of the converted LSF can be automatically normalized
    against suing the target LSF variance of the learning set.
    Moreover, a scalar can be used to control to which extent the
    normalization is applied. Zero means that no normalization is
    used and 1 means that the global variance of the converted LSF is
    set to that of the learning set. In SVP, this scalar can be chosen
    specifying a number after the file name of the conversion model
    using a comma, as in the example below.
    (default value is 0.95)


    Examples:

    svp -Afft -Z -Sxavier.wav -Fvconv c3po.model2 fernando.wav
    svp -Afft -Z -Sxavier.wav -Fvconv c3po.model2,c,0.95 fernando.wav

--------------------------------------------------------~

leccmd:usage-ffile-subtract =
sub, sub-det, sub-noex, sub-noex-det::  %s

   spectral subtraction  filter will subtract  time interpolated noise
   spectrum that has been created via -OM1/-OM2 %s output option from
   noise  segments  specified  via   -avseg  option  from  the  signal
   spectrum.  The   spectral  subtraction  allows   fine  control  via
   additional  command line  parameters:  %s, %s,  %s,
   %s, and %s.

   The parameter  file contains the spectral description  of the noise
   spectra for all the segments. According to the selected description
   mode the first  column contains the mean or  maximum value M(k) for
   each bin k and the  current segment. The second column contains the
   standard deviation  S(k) of the  bin for the current  segment.  The
   noise estimate for bin k is given by

   NE(k) = M(k) + 10^(avsfac/20) * S(k)

   If we  denote NI(k)  the interpolation of  the two  noise estimates
   from the  neighboring segments contained in the  parameter file the
   spectral  subtraction  module  would  create the  output  amplitude
   spectrum O(k) from the input amplitude I(k) spectrum following

%s
   O(k)  = max(0,Gl(k)*I(k)) with the generalized spectral subtraction factor
   G(k)  = max(sum_bark([1-[NI(k)/I(k)]^avgamma]^(1/avgamma)),10^(avbeta/20))
   Gl(k) = max(Gl(k) * exp(-td/(avrelax/1000)),G(k))
%s

   G(k) will be  close to 1 if the input amp.  spectrum is larger than
   the noise  estimate and  become small if  the input  amp.  spectrum
   approaches the  noise estimate. Gl  provides a means to  modify the
   decay rate of the spectral subtraction factor.
   
   To prevent  musical noise the  spectral subtraction factor  G(k) is
   always averaged over the bark band related to bin k.

   Meaning of command line parameters:
   ----------------------------------

%s
   avbeta   : controls remaining noise floor (maximum spectral attenuation
        is given by avbeta). 
        see: control flag %s
   avsfac   : controls impact of standard deviation on the noise estimate
        and establishes a means to obtain over/under-compensation. 
        see: control flag %s
   avrelax : do not decrease Gl(k) faster than with an 
        relaxation time constant. avrelax is given in milliseconds. 
        see: control flag %s
   avgamma : switches between amplitude (gamma=1)
        and energy (gamma=2) subtraction.  
        see: control flag %s
   avdet : alternative interface to enable and disable deterministic/sinusoidal noise 
        subtraction. see: control flag %s
%s
   
   Deterministic noise:
   ====================

   By adding  the flag  -det to the  filter type special  treatment of
   stationary sinusoids in the noise estimate is requested.  Sinusoids
   are detected from the averaged spectrum and removed below the noise
   level prior  to applying  spectral subtraction. The  %s flag  is an
   alternative    means   to    control   the    deterministic   noise
   subtratcion. This flag has priority of the -det filter modifier.

   Extrapolation:

   In normal operation the first  and last lines of the parameter file
   are  extrapolated without changes  to start/end  of sound.   If the
   flag -noex  is appended to the  filter type the filter  will not be
   extrapolated but  only applied inside the time  limits specified in
   the parameter file
%s
-------------------------------~

leccmd:usage-f-avdet = 
-avdet <value> ::          %s                 (Def: 0)

   Controls the  deterministic noise  mode of the  subtraction filter.
   If value is zero than  the deterministic noise handling is switched
   off, if  value is  not zero it  is switched on.   The deterministic
   noise  setting specified with  the -avdet  flag overrules  the -det
   modifier of the subtraction filter.
   	
   ---- Example: -avdet 1

   Enable deterministic noise subtraction.

   See: sub filter module %s. 

-------------------------------~

leccmd:usage-f-avbeta = 
-avbeta <noise floor value> ::          %s                 (Def: 120dB)

   Specifies maximum noise suppression factor for spectral subtraction
   filter. The value is specified in dB attenuation.
   	
   ---- Example: -avbeta 20

   Limits  noise reduction in  the spectral  subtraction filter  to be
   less or equal -20dB.

   See: sub filter module %s.  

-------------------------------~

leccmd:usage-f-avgamma = 
-avgamma <1 or 2> ::                  %s                        (Def: 1)

   Specifies  amplitude or  energy subtraction  mode for  the spectral
   subtraction filter.

   ---- Example: -avgamma 2

   Selects  energy  control  instead  for amplitude  control  for  the
   spectral subtraction filter.

   See: sub filter module %s. 
	
-------------------------------~

leccmd:usage-f-avsfac = 
-avsfac <standard deviation multiplicand> ::    %s             (Def: 0)
       
   Specifies the factor to  multiply with the noise standard deviation
   for  estimating  the  noise  amplitude  spectrum  in  the  spectral
   subtraction filter.

   ---- Example: -avsfac 1.

   Adds  one time  the standard  deviation  to the  mean amplitude  to
   obtain the noise estimate.

   See: sub filter module %s. 
	
-------------------------------~

leccmd:usage-f-avrelax = 
-avrelax <relaxation time constant> ::        %s              (Def: 0.)

   specifies the time constant in  ms for the decay of the generalized
   filter factor in the spectral subtraction filter.

   ---- Example: -avrelax 20.

   Relaxation time  constant of generalized subtraction  filter set to
   20ms.

   See: sub filter module %s. 
	
----------------------------------------------------------------------~
   
leccmd:usage-G =
-G<cross-synthesis type> [filename] :: or %s
-G<cross-synthesis type> [-X<val>] [-x<val>] ......
   ... [-Y<val>] [-y<val>] [-q<val>] ::    (Def.: X=1,x=0,Y=1,y=0,q=0)


   Combine the two  different tracks into a single  track. Suppose S1,
   S2 are the  short time FT spectra of the two  input channels and SO
   is the  resulting spectrum of the  cross-synthesis and X/x/Y/y/qval
   is  the  value  specified  with  flag  -X/x/Y/y/q  then  the  mixer
   operation is for

   cross type add: transform input spectra into representation
	  using real/imaginary part and calculate
%s             
	  SO = Xval * S1 + xval * S2,
%s
   cross type cross: S1 and S2 are calculated in amplitude/freq
   	  representation and the output is

%s	  Amp(SO) = Xval*Amp(S1) + xval*Amp(S2) +qval*(Amp(S1)*Amp(S2))^CE
	  Fre(SO) = Yval*Fre(S1) + yval*Fre(S2),
          
          the exponent CE is by default 0.5. It can be modified by means
          of the flag %s.
%s
   cross type mul: (source-filter), S1 and S2 are transformed 
        into Amp/Phase representation and the output is

%s	     Amp(SO) = Amp(S1)*Amp(S2)
	     Pha(SO) = Pha(S1)+Pha(S2),
%s

   cross type amul: (source-filter), S1 and S2 are transformed 
        into Amp/Phase representation and the output is
	       
%s	     Amp(SO) = Amp(S1)*Amp(S2)
	     Pha(SO) = Pha(S1),
%s

   cross type pmul: (source-filter), S1 and S2 are transformed into
        Amp/Phase representation and the output is
	       
%s	     Amp(SO) = Amp(S1)
	     Pha(SO) = Pha(S1)+Pha(S2).
%s
   filename is the parameter file for add and cross modes.

        for cross mode the file is made of lines with: time X x Y y q
	for add mode the file is made of lines with: time X x

   Notes:	
   
   Order and position of the -XxYyq flags is free.

   The processing is  stopped in cross and add mode if the sound file in the first
   track is finished, in mul/amul/pmul mode if either sound file is finished.

   Note  that the increment  step of  the second  track is  adapted to
   match the duration  of both sources if you do  not specify the step
   size for the second channel with -i explicitly.~

leccmd:usage-G2 =

   ------ Example : -Gadd file
   ------ Example : -Gadd -X1.0 -x2.0
   ------ Example : -Gcross -X0.5 -x0.5 -Y1.0 -y1.0 -q0.0

   to obtain a normalized excitation signal (with flat/white envelope)
   you should use mul mode and inverse envelope estimation (see: %s or
   %s) and the same sound file in both channels as follows
   
  ------ Example : supervp -Ssound_file -ssound_file -Gmul -Z -Afft -atenv_inv 50Hz output_file

----------------------------------------------------------~

leccmd:usage-g =
-ggain <gain_factor or filename> :: or 
-gtremolo <mode> <filename>    :: or 
-gfilter <filter_parameters or filename>::  


   gain <factor or filename>:

        multiplies output samples with envelope specified 
        in file file contains lines with time envelope, anything not starting
        with [-][.]digit is considered to be a parameter file.
        As always [] indicates an optional element.

   tremolo <mode> <filename> :

           mode is the type of amplitude modulation
%s
            sinus
            carre (square wave)
            triangle
            scie (sawtooth wave)
            file contains lines with time depth
%s

   filter <parameters or file> :

	filter signal using a linear filter that implements

	o(n) = sum_i b_i x(n-i*K) - sum_j a_j o(n-j*L).

        the filter parameters b_i,a_j are either specified directly
	on the command line or in the parameter file.

	The format is : K,b_0,b_1,...;L,a_0,a_1,...

	If a_0 is not equal 1 all a_i are normalized by a_0. 
        The semicolon can be replaced by means of a slash /
        
  ------ Example : -ggain file
  ------ Example : -gtremolo sinus file
  ------ Example : -gfilter 1,1,0.2;3,1,0.5

---------------------------------------------------------------------~

leccmd:usage-oversamp =
-oversamp <val> ::         %s                                  (Def: 8)
   
   specifies the  minimum overlap  of analysis and  synthesis windows.
   The maximum  increment between  successive windows is  specified in
   terms of  parts of the  window size.  Smaller values  indicate less
   overlap   which  results   in   less  quality   but  also   reduced
   computational costs.  The default  value (increment not larger then
   an  8-th part  of the  window) ensures  very high  quality  for all
   situations.  Values above 4  will be almost always sufficient.  For
   values below 4 artifacts are  unlikely but may occur. Minimum value
   is 2.

   ------ Example : -oversamp 4
        
----------------------------------------------------------~

leccmd:usageoutput =
======================================================================
============================  Output options%s ========================
~

leccmd:outputfile = 
<output filename> :: 
   
   The outputfile is specified without option switch as last parameter
   of the command line. If no  output file is specified or the name of
   the file is "stdout" the result is directed to stdout.  if the name
   of the output  file is "@PLAY@" the output is not  stored as a file
   but directly  played as  sound via the  audio hardware  The default
   latency is  0.5 sec and can be  changed by means of  adding a float
   number to the play file  name. For example @PLAY@0.2 would indicate
   play the sound with latency 0.2 sec.

--------------------------------~

leccmd:usagebody-O =
-O<mode>:<list> ::  or
-O<mode> <file> :: %s                                  (Def: mode==b)

  <mode> is the output format

  SOUND output formats :
%s
   sa  for AIFF/AIFC  16bit integer soundfile format.
       sa8,sa16,sa24,sa32 select the different sample size in bits
       saf,sad  select float or double samples in AIFC format.
   sA  equivalent to sa.
   sis for Ircam 16bit integer soundfile format.
   sif for Ircam 32bit float   soundfile format.
   sw  for wav 16bit integer sound file format.
       sw8,sw16,sw24,sw32  select the different sample size in bits
       swf,swd select float or double samples in WAV format.
   sW  equivalent to sw.
   sn  for NeXT (.au)  16bit integer soundfile format.
       sn8,sn16,sn24,sn32  select the different sample size in bits
       snf,snd select float or double samples in NEXT (.au) format.
   ss  for Mac Sounddesigner II format (MacOSX only)
   srs for raw   16bit integer soundfile.
   srf for raw   32bit float   soundfile.
   sf  for FLAC soundfile format.
       sf8,sf16,sf24 select the sample size in bits
%s
   IMPORTANT : default format and packing mode (integer or float) differ 
   according to the output type (a file or a pipe):
%s
   	File output : those of the input processed sound (not normalized).
   	Pipe output : raw 32bit float (not normalized).
%s
~

leccmd:usagebody-Odef =
  ANALYSIS Output Selector : %s
%s 
   a      : for ASCII analysis data output 
   b      : for binary analysis data output (default) 
   g0     : Unified file format, data is log amplitude stored as unsigned char
   g1,g2,g3,g4,g5,g6
          : Unified file format, see below and gabarit filter
   S0,S1,S2,S3,S4,S5,S6 
          : Unified file format stored as SDIF file 
   T      : sdif file containing description of attack transients
            use 'TE' to obtain extended transient information containing
            for each attack transient the frequency regions that are 
            covered by the attack.

   F+,F-  : sdif  file containing  local maxima  of  averaged spectral
            differences  (add selects regions  with increasing  (+) or
            decreasing   (-)  amplitude  for   difference  evaluation,
            (Def:+)
	           
   M1,M2  : averaged spectral  information for all  segments specified
            via -avseg  flag. Output is stored  in an SDIF  file in an
            1AVS/1AVS Matrix following an IAVS frame denoting analysis
            infos.   The  M1  mode   outputs  the  mean  and  standard
            deviation of  the absolute spectrum  for each bin,  the M2
            mode outputs  maximum and  standard dev.  of  the absolute
            spectrum for  each bin. Note, that the  power spectrum can
            be derived from M1 results by (mean*mean + stddev*stddev)!

   E :      output for envelope  data using envelope  coefficients  in
            SDIF frames  of type 1CEC/1ARR  respectively.  Note:  that
            envelope scaling operations performed with -envscal or the
            request for spectral normalization  will only be partially
            reflected in the generated output.

            cepstral coefficients (-Atenv)
            ==============================
            
            -ns  flag: In  this case  the coefficents  will be  scaled
            exactly as  the output spectrum generated  with -OS1. This
            means  that the  spectrum is  devided by  the sume  of the
            analysis window.
            
            -envscal flag: In this case the cepstral coefficients will
            reflect only  a part of  the scaling observed in  the -OS1
            output  The coefficients  are  in fact  scaled  as if  the
            envscal flag would have been given as -envscal 1,1.
            This means that the spectral envelope is normalized by the
            sqrt of the sum of squares of the analysis window.
            
            AR coefficients (-Alpc, -Adap)
            ==============================

            The coefficients always remain  standardised such that the
            mean log  spectrum is 0.   The scaling will be  applied to
            the  gain  matrix  that  is present  in  the  1ARA  frame,
            following the  same simplifications  as described  for the
            cepstral coefficients.

            Mixing  the scaling  flags -ns  and -envscal  on a  single
            command is currently not supported.
            
   G      : output Rd shape parameter of the Liljencrants-Fant model
            using the MSPD2 based method. If 'G' is followed by 'c',
            Glottal Closure Instants (GCI) are also estimated and Rd
            values are returned only at the GCI times.

   L      : output Line Spectral Frequencies (LSF) from True-Envelope
            estimation. 'L' can be followed by 'f' or 'd' for single
            or double precision respectively. Then, the LSF order is
            given (e.g. -OLd30). An additional option 'c' or 'n'
            allows to selected to used method. 'c' stand for
            "companion matrix" (default) whereas 'n' stands for
            "Newton". Note that, despite the Newton's way is a lot
            faster, artifacts can be visible for an order >50 and
            the method can be unable to compute LSF for higher order.
            Thus, the Newton's way is advised only for real-time
            computation with low-order.

   V      : output of voiced/unvoiced frequency limit (SDIF  1VUN/1VUF 
            format)

   f1,f2,f3,f4
          : sdif file containing  1FQ0  frames for f0 estimates.   The
            number specifies how many  columns will be present in each
            matrix out of  the possible four:  Frequency,  Confidence,
            Score and RealAmplitude (Def:1)
%s 
~

leccmd:usagebody-Oformat = 

   Format ( binary  file + header like gabarit file  ) using FFT, LPC,
   CEPSTRE.   File content  is  respectively t,8-bit  logamp; t,amp  ;
   t,phase ; t,amp,phase ; t,freq,amp ; t,pr,pi; t,amp for the gabarit
   modes 0-6.

   g1,g6,g2 formats  work with gabarit  filtering and g4,g5  ones work
   with the  synthesis module with  data on input channel.   g3 format
   works with both.  For a complete description of  the gabarit format
   see -Fgabarit filter.

   Use these format specifiers with -Oxx  option  (no  further  format 
   arguments)

   ------ Example : -Og3

   Using other  format arguments  will print a  warning but  the right
   output format will be set.

   <list>  is  a  list  of  strings separated  by  ','  number,  time,
         amplitude, frequency, phase, etc..
 
   <file> is a text file where output format is specified

    ------ Example: -Oa:number,amplitude,phase
    ------ Example: -Oa foo.format

----------------------------------------------------------------------~

leccmd:usagebody-Odatatable = 

List of available strings as output data

%s
	t,time: time (of frame)
	n,number: number (of entries per frame)
	a,amp,ampl,amplitude: linear amplitude
	adb,amp_db,ampl_db,amplitude_db: amplitude in dB
	pr,partie_reelle: real part of a complex number
	pi,partie_imaginaire: imaginary part of a complex number
	f,fhz,freq,frequence: frequency in Hz
	midicents,fmc: frequency in midicents
	i,ind,index: frequency as fft-index 
	chan,chan_freq: center frequency of fft-channel  
	ph,phase: phase
	spl: sound press excess
	weight: harmonic weight
	tsp,truesp,truepitch: harmonic true pitch
	sc,score: score
	coeff,coeff_pitch: pitch confidence
	largeur,largeur_formant: formant width
	cf,coeff_filtre: autoregressive lpc filter coefficients
%s

------------------------ Allowed combinations -----------------------
%s
                 fft    ced/tenv    mask    f0    formant   pic   lpc
index            OK,       OK,       OK,    NO,     NO,	    OK,    OK
chan_freq        OK,       OK,       NO,    NO,     NO,	    OK,    OK
partie_reelle    OK,       OK,       NO,    NO,     NO,     OK,    OK
partie_imagin    OK,       OK,       NO,    NO,     NO,     OK,    OK
frequence_hz     OK,       OK,       OK,    OK,     OK,     OK,    OK
frequ_cents      OK,       OK,       OK,    OK,     OK,     OK,    OK
frequ_midicents  OK,       OK,       OK,    OK,     OK,     OK,    OK
frequ_inst       OK,       OK,       NO,    NO,     NO,     OK,    OK
amplitude_lin    OK,       OK,       OK,    OK,     OK,     OK,    OK
amplitude_dB     OK,       OK,       OK,    OK,     OK,     OK,    OK
phase            OK,       OK,       NO,    NO,     NO,     OK,    OK
sndpressexcess   NO,       NO,       OK,    NO,     NO,     NO,    NO
truepitch        NO,       NO,       OK,    NO,     NO,     NO,    NO
weight           NO,	   NO,       OK,    NO,     NO,     NO,    NO
score f0         NO,       NO,       NO,    OK,     NO,     NO,    NO
coef pitch       NO,       NO,       NO,    OK,     NO,     NO,    NO
forma bandwidth  NO,       NO,       NO,    NO,     OK,     NO,    NO
coeff filtre LPC NO,       NO,       NO,    OK,     NO,     NO     OK
%s
~

leccmd:usage-nn =
-nn ::  

   do not normalize output sound file (this is default now)!
   ATTENTION : The use of -nn is strongly discouraged. Since version 1.75
   no normalization is the default behavior and -nn flag is no 
   longer supported and will be switched of in future versions!
   See -norm flag
---------------------------------------------------------------------~

leccmd:usage-inplace =
-inplace :: 
   
   stores results  into input file. Especially  useful when processing
   selected segments (-B/-E)  in which case only part  of the sound is
   processed and the result is put back into the input file.

   Normalization  is not  available during  inplace  processing.  When
   inplace processing is used with time dilation (-D) the selection of
   the end of  the segment should be carefully  selected such that the
   phase   shift   due   to   time   dilation   will   not   lead   to
   artifacts. Therefore a low amplitude region should be used.

   ------ Example: supervp -Sfile.aiff -inplace -Fbande-noex band.par -B1. -E2.
   
   Will apply  the band filter  with parameters band.par to  the sound
   file.aiff.  Only  the segment  from  time  1 sec  up  to  2 sec  is
   processed and the result is put back into the original sound.

---------------------------------------------------------------------~

leccmd:usage-norm =
-norm [level] ::                                             (Def.: 0)
   
   normalize output sound file to  level dB below maximum range of the
   output  data type.  0dB  normalization produces  data in  the range
   between +/- 1.0  for floating point sample format  and in the range
   between +/- (2^(N-1)-1 for N Bit integer sample format.

   IMPORTANT :  when normalizing, SuperVP uses a  temporary file. It's
   length is  the same length of  the processed sound  file when using
   32bit floating  point packing mode  or twice the length  when using
   16bit  short packing  mode.  Temporary  file is  created  in SVPTMP
   (setenv  SVPTMP  mytemporarydirectory).  When  SVPTMP  is not  set,
   SFDIR is used.  If SFDIR is  not set, current directory is used. If
   the  chosen  directory is  not  writable,  then  /var/tmp is  used.

---------------------------------------------------------------------~

leccmd:usage-ns =
-ns :: 
   
   requests spectral normalization  for analysis output, which results
   in  amplitude  values  to   be  below  1  (Def:  no  normalization)

---------------------------------------------------------------------~

leccmd:usage-D =
-D[coefficient] or -D<filename> ::                            (Def.: 1)

   constant or time-varying time-stretching

   Apply time dilation to input sound. 

   ---   Examples: -D2.0  (dilation of 2, duration is doubled)
                   -D0.5  (compression, duration will be divided by 2)

   parameter  files are used  for time  varying operations.  The files
   contain multiple lines indicating:

     time dilate_coefficient

   The entries are linearly  interpolated to derive the time dependent
   dilation. The  dilation coefficient before the first  and after the
   last file entry is extrapolated keeping its value constant.  The -D
   flag may  be given  more than once  in which  case the result  is a
   multiplication of the requested individual values.

   Time varying dilation and transposition  may be applied in a single
   SuperVP  call in  which case  the requested  dilation and  the time
   compensation for the transposition are added.

   The optimal analysis step size is calculated automatically if no -I
   option   is   specified.  This   is   recommended  especially   for
   time-varying dilation/transposition.


---------------------------------------------------------------------~

leccmd:usage-trans =
=====================================================================
========================== Transposition %s============================

-trans <cents> or <filename>  ::  %s 
   
   transposes by the given number of cents applying time correction to
   compensate  the  duration  effects  of transposition  by  means  of
   resampling.  Transposition  requires the -A and -Z  flag.  For time
   varying  transposition  a  parameter  file  has to  be  used  which
   contains lines with:

     time transposition

   Which are linearly interpolated.  Since SuperVP 2.40 the trans flag
   may  be  given   more  than  once  in  which   case  the  effective
   transpositions  are added.  transpositions are  always extrapolated
   outside the range specified in the parameter file.
    
   ------ Example: -trans 1200 (will transpose one octave up)

--------------------------------
   
-transenv <cents> or <filename> ::  %s

   transpose the spectral envelope only, leaving the pitch unchanged

--------------------------------
   
-transke <cents> or <filename> ::   %s

   transpose as -trans, but, preserves spectral envelope making use of
   the envelope estimation selected via -Afft and the -env... flags.

--------------------------------
   
-transmo <mode> ::                %s                       <Def.: 0>

   Select transposition  algorithm to be used for  -transke and -trans
   operations.   Since SuperVP  2.92.49 two  different  algorithms for
   transposition are  available. The  first, and in  previous versions
   the  only  one,  is  based  on time  domain  resampling  with  time
   correction.   The  second one  transposes  by  means  of forming  a
   sinusoidal model, modifying  the sinusoidal parameters according to
   the  required  transposition  and  resynthizing the  the  sinusoids
   directly in the spectral domain.

   Note, that  the time  based algorithm has  still a  slightly higher
   quality if compared to the frequency domain algorithm.  The special
   advantage of  the algorithm  working with sinusoidal  parameters is
   the fact  that, in  contrast to the  time based algorithm  that has
   calculation  costs  linearly   increasing  with  transposition  for
   transposition  factors  above one,  the  calculation  costs of  the
   frequency domain algorithm do not depend on the transposition to be
   performed, and  are roughly equivalent to the  calculation costs of
   transposing 400cents up with the time domain algorithm.

%s
   -transmo 0 : (the default) selects time domain transposition
   -transmo 1 : selects automatic, transposition  dependent
                of the transposition algorithm. Transposition will
                by frequency domain based for transposition up
                and time domain based for transposition down.
   -transmo 2 : selects frequency domain transposition.

%s
                
   ------ Example: -transmo 1 -trans 1200 

   will transpose one octave up by means of modifying the sinusoidal
   parameters of the signal.

--------------------------------
   
-transnc <cents> or <filename> ::  %s

   Applies transposition without time correction.  The perceived pitch
   is changed by means of  sample rate conversion without changing the
   samplerate  of the resulting  signal. Therefore  the length  of the
   sound will  be changed. The  parameters or parameter files  and are
   equal to  the -trans flag. -trans  and -transnc flags  may be given
   more  than once  even  with  overlap in  which  case the  requested
   transpositions are added but will only be partly compensated.

   ------ Example: -transnc 1200 (will transpose one octave up)

--------------------------------
   
-trmdle <minFreq>[-<maxFreq>],<minAmp>[-<maxAmp>][,interval]

   Modulates envelope while preserving the pitch as per -transenv. The
   modulation frequency and amplitude  can be either fixed or randomly
   generated within a range.

%s
   minFreq   : lowest modulation frequency in Hz
   maxFreq   : highest modulation frequency in Hz (Def: minFreq)
   minAmp    : lowest modulation amplitude in cents
   maxAmp    : highest modulation amplitude in cents (Def: minAmp)
   interval  : randomized  parameters,  if any,  are applied  for that
               duration (in seconds)  before new values  are generated
               (Def: 0.2)
%s

   ------ Example: -trmdle 20,50
   ------ Example: -trmdle 20,40-60,0.4
   ------ Example: -trmdle 18-22,50
   ------ Example: -trmdle 18-22,40-60,0.25

--------------------------------
   
-trmdlp <minFreq>[-<maxFreq>],<minAmp>[-<maxAmp>][,interval]

   Modulates pitch while preserving the envelope as per -transke.  The
   modulation frequency and amplitude  can be either fixed or randomly
   generated within a range.

%s
   minFreq   : lowest modulation frequency in Hz
   maxFreq   : highest modulation frequency in Hz (Def: minFreq)
   minAmp    : lowest modulation amplitude in cents
   maxAmp    : highest modulation amplitude in cents (Def: minAmp)
   interval  : randomized  parameters,  if any,  are applied  for that
               duration (in seconds)  before new values  are generated
               (Def: 0.2)
%s

   ------ Example: -trmdlp 20,50
   ------ Example: -trmdlp 20,40-60,0.4
   ------ Example: -trmdlp 18-22,50
   ------ Example: -trmdlp 18-22,40-60,0.25

----------------------------------------------------------------------~

leccmd:usage-tr-filt =
-tr_filt <frequency boundary>:: %s                       (Def: 0 =off) 

  Apply  band  stop  filter  to  remove low  frequency  energy  before
  transposition. The  filter is  not a general  purpose filter,  it is
  applied only if unperceived signal components become perceivable due
  to transposition upwards.  The filter frequency parameter can either
  be specified  as a constant frequency in  Hz or as a  factor for the
  signals  fundamental  frequency.  If  the  frequency  boundary is  a
  positive  number it  is  interpreted as  frequency  in Hz  if it  is
  negative  it  is  interpreted   as  a  factor  that  multiplies  the
  fundamental frequency  to establish the  frequency boundary.  Before
  multiplication  the negative sign  is removed.   The second  case is
  possible only if the the fundamental frequency has been specified on
  the command line (see: %s).
  
   ------ Example: -tr_filt 50

  Remove signal components below 50Hz when transposing up.

----------------------------------------------------------------------~

leccmd:usage-U =   
-Uu ::                     
   
   controls the position of the  first analysis window with respect to
   the first sample for both tracks.

   If  the -U/-u  flag is  not given  the processing  starts  with the
   analysis window centered over the first sample.
   
   With -U/-u moves the window  such that its first sample matches the
   first sample of the sound. This avoids artifacts due to the initial
   step in  signal amplitude. For  time stretching and  processing the
   sound will be ramped in, but,  a zero value range may be created if
   time stretching is applied.
   
----------------------------------------------------------------------~

leccmd:usage-vufintro =

======================================================================
==============  Voiced/Unvoiced Cut-off Frequency%s ===================

SuperVP can be  used to estimate and output  a voiced/unvoiced cut-off
frequency  VUF.  This  cut-off frequency  is used  in  shape invariant
processing  mode.  The  VUF  that  will be  used  for shape  invariant
processing  can  either  be  internally calculated  or  specified  via
command line (see %s).

If  the V  option  is  given to  the  -O flag  the  VUF is  internally
calculated and output into an SDIF file.

The basic  idea of the  algorithm is that  only a small amount  of the
spectrum  of unvoiced  bands should  be related  to  sinusoidal peaks.
Experimental evaluation  shows that vuf  estimation is more  robust if
the contribution  of sinusoids is  not measured in terms  their energy
but in terms of the area under the amplitude spectrum (AAS).

Accordingly, frequency  bands are classified as  unvoiced whenever the
relative  sinusoidal  AAS  (RSAAS),   which  is  the  AAS  related  to
sinusoidal peaks normalized  by the AAS in the whole  band, is below a
threshold.  The  band size is controlled by  the parameter -vu_nument.
The threshold range is adjusted automatically to take into account the
random misclassification of noise components as sinusoidal components.
The  threshold  is determined  as  follows. First  the RSAAS  due  to
misclassification  of noise  (see  %s )  is determined  using a
small segment of pure noise data.   The RSAAS level of the majority of
these bands is considered as the lower end of the range of RSAAS for a
voiced band. The  upper end of the range of RSAAS  for voiced bands is
1.  The RSAAS  threshold for  voiced bands is  controlled by  the user
parameter %s.

Note, that sinusoidal detection  is performed differently in the first
VUF analysis band. Because for  speech signals it happens rather often
that the only sinusoid is a  rather weak fundamental the first band is
classified as sinusoidal  if it contains at least  one sinusoid and if
the  amplitude of this  sinusoid is  at least  -20dB below  the frames
maximum amplitude.
  
The different flags that effect the VUF estimation are:
%s
  -Vuf        - specify VUF  via command line or BPF file, or modify 
                VUF smoothing parameter. %s
  -vu_errlev  - error tolerance for noise/sinusoidal peak 
                classification %s
  -vu_nument  - select size of voiced/unvoiced detection sub-bands %s
  -vu_rth     - threshold for RSAAS in sub-bands 
                classified as voiced  %s
  -vu_f0lim   - control vuf detection based on sinusoidal partial
                positions %s
  -vu_fr      - frequency boundaries for linear increase of sinusoidal 
                energy threshold   %s
  -vu_fac     - constant factor that  be applied to estimated vuf %s
  -vu_dem     - control preprocessing steps for sinusoidal/noise peak
                classification. %s
  -vu_remix   - detect sinusoidal and noise peaks and remix using
                given factors. %s
  -vu_res     - enable to use complete sinusoidal model for remixing %s
  -vu_peakst  - set parameters for table pre-caluclated sinusoidal 
                peaks %s
  -vu_rep     - replace strongly amplified peaks in shape invariant
                mode %s
%s
For  further  information   about  the  use  of  the   VUF  see  phase
randomization  switches (see %s).

--------------------------------~

leccmd:usage-vuf =

-Vuf <value> or <bpffile> ::      %s                       (Def.: -4 ) 

   the Vuf  parameter serves to specify  the voiced/unvoiced frequency
   limit  in  Hz above  which  phase  randomization  according to  the
   parameters   (%s/%s)  should   be   applied.   Phase
   randomization is applied during  synthesis, therefore the -Vuf flag
   has no  meaning if no  -Z flag is  given. The argument is  either a
   fixed value or a breakpoint  function file that contains time value
   pairs  with the  frequency  limit  specified in  Hz.   If value  is
   negative the  vuf frequency  is estimated on  the fly  and smoothed
   with a  median filter covering analysis  frames up to  an offset of
   +/-value*window_length/8.    Accordingly  the  parameter   -Vuf  -4
   demands   median    smoothing   of   the    internally   calculated
   voiced/unvoiced frequency  limit over  a signal segment  having the
   size of the analysis window.

   If the  shape invariant processing  has been requested and  no -Vuf
   flag  has been given  the voiced/unvoiced  frequency limit  will be
   automatically estimated using a default value of -Vuf -4.

---------------------------------

---------------------  VUF estimation parameters ---------------------~

leccmd:usage_vu_errlev = 
-vu_errlev <value> ::    %s                                (Def.: 0.1)
   
   Select sinusoidal/nonsinusoidal  peak classification such  that the
   given  amount  of  noise  peaks  are  misclassified  as  sinusoidal
   peaks. Note, that for  non stationary sinusoids the distribution of
   the features  overlap the distribution of noise  peak features. The
   impact   of  this   parameter  is   rather  weak   to   change  vuf
   classification better use %s.
   
   ----- Example : -vu_errlev 0.2
  
-----------------------------------~
leccmd:usage_vu_nument =
-vu_nument <value> ::   %s                                   (Def.: 6)   

   determine  size of  spectral bands  used to  detect voiced/unvoiced
   frequency regions.  The value given  selects the band size in terms
   of the width of the  mainlobe of a stationary sinusoid.  Increasing
   the size of  the band will improve the  statistical validity of the
   voiced/unvoiced   classification,    however,   will   reduce   its
   sensitivity.

   ----- Example : -vu_nument 10
   
   Increase   size  of   spectral  bands   used   for  voiced/unvoiced
   classification to cover 10 stationary sinusoids.
   
-----------------------------------~
leccmd:usage_vu_rth =
-vu_rth <rel theshold value> ::   %s                        (Def.: 0.3)

   Set the threshold for the amount of relative sinusoidal area of the
   amplitude spectrum  (RSAAS) that  is required for  voiced frequency
   bands.   Frequency bands  are classified  as unvoiced  whenever the
   area under the amplitude spectrum (AAS) related to sinusoidal peaks
   normalized by the AAS in the whole band is below a threshold.

   The relative  threshold value is  in the range  0 to 1.   The RSAAS
   level that  a white noise  signal will only  achieve in 10%%  of the
   cases is  considered as the lower end  of the range of  RSAAS for a
   voiced  band. If  the user  selected relative  threshold is  0 this
   RSAAS  is the  threshold for  voiced bands and in this case  10%% of
   misclassifications of voiced bands have to be accepted.  The larger
   the  threshold is  selected  the higher  the  amount of  sinusoidal
   amplitudes has  to be  for a  band to be  voiced.  If  the relative
   threshold is  1 the RSAAS  that is required  for voiced bands  is 1
   such that no single noise peak is allowed within a voiced band.

   This is  the main parameter to control  the voiced/unvoiced cut-off
   frequency estimation!   
   
   ----- Example : -vu_rth 0.9
   
   Requires nearly all peaks in  a band to be classified as sinusoidal
   for a band classified to be voiced.
   
----------------------------------~
leccmd:usage_vu_f0lim =
-vu_f0lim <par1[,par2]> ::  %s    (Def.: 1,Inf)
   
   if f0 information is available then the vuf estimation can make use 
   of the expected partial position of an harmonic signal to improve vuf 
   detection. the use of f0 info for vuf estimation can be disabled by means of
   setting bit 3 of the %s mode selector!
   
   Exceptions related to the fundamental frequency value:
%s
   1) the vuf will be forced to zero if the fundamental frequency provided
   is 0.
   2) the vuf will be forced to be above a sinusoidal peak if the peak
   is located at the partial position of a partial at or below the partial
   speified by parameter par1.
   3) the vuf will be forced tpo zero if the first partial that is detected
   as sinusoid is above the partial position specified by par2.
%s 
  
   ------ Example : -vu_f0lim 2,10

   force vuf to ly above sinusoidal peak if the peak is present at the location 
   of the first or second partial and force vuf to 0 if the first sinusoidal peak 
   is higher then the 10th partial position.

----------------------------------~
leccmd:usage_vu_fr =
-vu_fr <value[,value]> ::         %s               (Def.: 4000,12000Hz)
   
   sets  the  dynamic sinusoidal  energy  threshold control  frequency
   limits.  Generally  voiced bands are  more likely in  low frequency
   regions.   To increase  robustness of  VUF estimation  the relative
   sinudoidal energy threshold will  increase with frequency such that
   voiced/unvoiced classification will  be increasingly biased towards
   unvoiced.   Up to  the first  value given  the  relative sinudoidal
   energy  threshold  as specified  via %s  flag  is used.   For
   frequencies above the second value the detection of voiced bands is
   disabled.   For intermediate  frequencies  the relative  sinudoidal
   energy thresholds linearly increases from its start value to 1.  If
   only  one value  is  specified  this value  is  interpreted as  the
   maximum frequency for for  that the user supplied sinusoidal energy
   threshold should be used.
   
   ------ Example : -vu_fr 6000,19000


----------------------------------~
leccmd:usage_vu_fac =
-vu_fac <value> ::     %s                                    (Def.: 1)
   
   constant factor that will be applied to estimated vuf. value has to
   be in the range [0, 40000]. Can be used to scale the estimated vuf. 

   
   ------ Example : -vu_fac 1.5

   scales vuf up by 1.5

----------------------------------~
leccmd:usage_vu_dem =
-vu_dem <mode> :: %s                                         (Def.: 0)
   
   requests local  signal normalization prior to  vuf estimation.

   The   time  variation   of  sinusoidal   amplitude   and  frequency
   trajectories  renders  the  classification  of the  sinusoids  more
   difficult. To  reduce these  disturbing impacts the  vuf estimation
   module allows  to apply local amplitude  and frequency demodulation
   before the actual vuf estimation is performed.  Both operations are
   controlled by  the mode  variable given as  argument to  the vu_dem
   flag. The vu_dem  argument is an integer containing  mode bits, the
   meaning of the different bits is as follows:

%s 
    bit 0 : enables demodulation of f0 evaluation prior to
            sinusidal peak classification 
    bit 1 : enables amplitude demodulation of the signal using
            local linear amplitude model for the complete signal
    bit 2 : unused
    bit 3 : disables the special vuf mode for the peak at the fundamental
            frequency. 
%s

   Note that f0 demodulation will only be performed if the %s flag is
   specified  and  an  external  estimate  of  the  F0  trajectory  is
   available. For amplitude demodulation no -F0 parameter is required.
   To  make  best  use  of  the reduced  varation  of  the  sinusoidal
   components the accepted  noise peak classification error %s
   should be lowered.
   
   In normal operation the  spectral peak at the fundamental frequency
   is  considered to  be  of  strong importance.   When  this peak  is
   classified as  sinusoid it will  automatically force the Vuf  to be
   located above  its upper minimum  without concidering the  RSAAS of
   the related band. This secial treatment can be disabled by means of
   bit 3, which, if set, suprresses the special treatment for the peak
   located  at   the  F0.    See  %s  parameter   for  detailed
   information about these special treatments for vuf estimation.

   Note,  the  sampling  of  the   F0  trajectory  that  is  used  for
   demodulation is controlled by the %s parameter.

   ------ Example :   -vu_dem 1  -F0 f0.file   

   demodulate the F0 trajectory as given in f0.file  

   ------ Example :   -vu_dem 2   

   demodulate  signal amplitude evolution in current signal frame
   prior to peak classification for the VUF estimation.

----------------------------------~
leccmd:usage_vu_remix =
-vu_remix <sinfac,noisfac>  ::  %s                           (Def.: off)

   enables classification of spectral  peaks into sinusoidal and noise
   peaks and applies  given factors to the two  classes.  The remixing
   factors may  be combined with the transient  remixing factors given
   via -td_ampfac  and -td_relaxto. Note, that in  this case transient
   peaks  are  never  considered  as  sinusoidal or  noise  and  both,
   sinusoidal as well  as noise peaks, are mixed  using the product of
   -td_relaxto  and  there  respective  class  factor.  If  sinfac  is
   negative remixing  is disabled. If noisfac is not given the default
   value "1" is used.

   -----  Example : -vu_remix 2

   multiplies all  sinusoidal components by factor 2,  does not change
   noise components.

----------------------------------~
leccmd:usage_vu_rep =
-vu_rep [value]  :: %s                                      (Def.: off)
        
   replace peaks at harmonic positions below VUF that either have been
   qualified as non sinusoidal  or that will be strongly amplified due
   to transposition  with envelope preservation.  These  peaks risk to
   introduce noise into the  voiced harmonics and, therefore, they are
   better  replaced  by  harmonic  peaks that  are  relatively  strong
   already in the original signal.

   The  peak replacement code relies on  the fact  that the  signal is
   monophonic  and  harmonic.  It  depends  on  the  knowledge of  the
   fundamental frequency and will therefore  work only if the -F0 flag
   has been given. Furthermode, the function is currently working only
   in shape invariant mode.

   The parameter value determines the maximum amplification factor (in
   dB) that a  peak is allowed to endure  due to envelope preservation
   operation.   A larger  factor, or  the fact  that the  peak  is not
   detected as sinusoid (see: %s) will trigger the replacement
   of this  peak by a stronger  harmonic. The replaced  peak will have
   the  same amplitude  as the  original peak  such that  it  will not
   modify the  timbre.  If  no parameter is  specified then  then peak
   replacement is enabled and the default parameter of 20dB is used.
 
   ---  Example : -vu_rep 6

   Enables peak replacement  for  peaks with more then 6dB (faktor 2)
   amplification.

----------------------------------~
leccmd:usage_vu_res =
-vu_res <bool>  ::   %s                                      (Def.: 0)
        
  Change sinusoidal/noise  remixing to estimate  sinusoidal parameters
  for  the  sinuosidal  peaks  and  disintiguish  the  sinusoidal  and
  resiudal  energy  in  the  region  of  the  sinusoidal  peaks.   The
  sinusoidal model is handled completely in the frequency domain using
  pre-calculated sinusoidal (see : %s )

  ---  Example : -vu_res 1   

----------------------------------~
leccmd:usage_vu_peakst =
-vu_peakst <N/maxSlope/minover>  ::  %s                (Def.: 200,5,2)
        
  the   sinusoidal  model   used  during   sinusoidal   remixing  uses
  pre-calculated  sinusoidal peaks for  remixing and  frequency domain
  transposition.  The table contains sinusoidal  peaks for a grid of N
  frequency  slopes in  the  range  [0 -  maxSlope].  maxSlope is  the
  frequency change over the duration  of the window length relative to
  the frequency resolution given by 1/windowSize.

  minover  represents  the  FFT   oversampling  factor  used  for  the
  sinusoidal  peak table.  This value  will  be increased  if the  FFT
  oversampling is above the minover value.

  maxSlope > 10 is hardly  ever useful because a frequency change over
  the  window of more  than 10  will -  depending on  the form  of the
  window- start  to destroy the mainlobe  of the peak.   For smaller N
  and smaller maxSlope the residual  will be less precise but the time
  necessary to calculate the tabe is smaller as well.

  ---  Example : -vu_peakst 1/0

  Keeps only the peak of a single stationary sinusoid in the table. 

---------------------------------------------------------------------~
leccmd:usage-stab-ar =
-stab_ar  [bw_limit][::bw_limits]  ::  %s                (Def.: off)
        
  stabilize autorgessive poles derived with  %s or %s type analysis or
  the   LPC  and   DAP   envelope  preservation   methods  (see   %s).
  Stabilization is  performed by means  of limiting formant  band with
  according  to  the  bpf  parameter  specified  as  bw_limits,  given
  independently for track  1 (in front of an  optional "::" separator)
  or 2 (after an "::" separator).

  bw_limits are specified as breakpoint  of comma separeted pairs of a
  bpf freq  in Hz and  the related bandwidth  limit in Hz.   The break
  point functions  a linearly interpolated and  extrapolated using the
  last valid  values. The special term  "def" can be used  to select a
  default bw_limitation breakpoint function that is equivalent to

  0,80/50,50/2000,50/4000,200

  If only a single term without "," and "/" separator is given it will
  be used as a constant bw limit for all frequencies.

  ---  Example : -stab_ar def::0,80/50,30

  Set default formant  bandwidth limitation for the first  track and a
  set a bpf decreasing between 0  and 50Hz from 80Hz to 30Hz bandwidth
  limit and remaining constant above 50Hz.

---------------------------------------------------------------------~

leccmd:usage-shape =
=====================================================================
=====================  Waveform preservation%s =======================
   
   Shape  invariant processing  is a  special mode  in that  the phase
   vocoder algorithm  is modified to preserve  phase relations between
   all the  sinusoidal components of  a source sound  signal.  Because
   the phase relations are preserved for all sinusoids the waveform of
   the  signal will  be preserved.   This waveform  preservation  is a
   reasonable shape  invariant processing mode can only  be applied to
   sounds  containing  only  one   sound  source.  Because  the  phase
   relations  are essential  for  speech signals  the shape  invariant
   processing   is  especially   suited  for   processing   of  speech
   signals. Shape invariant  mode requires a number of  sub modules to
   work correctly.   Essential are the  voice/unvoiced frequency limit
   (see %s),    the   phase   randomization (see %s). 

----------------------------------------------------------------------
-shape <0/1/2/4/...> ::   %s                                  (Def.: 0)
   
   Enables shape invarant processing.
   
   The argument given to the  shape flag determines the precision that
   is used for  phase synchronization. The argument has  to be a power
   of 2.   If the  argument is  0 the shape  invariant mode  is turned
   off.  For argument  1  phase synchronization  is  performed with  a
   precision of  one sample.  For argument 2  the precision is  half a
   sample.    Increasing   the   precision  will   require   increased
   computation without significantly affecting the results.
   
   For  proper  processing  of  the  unvoiced  signal  components  the
   voiced/unvoiced cut-off frequency is required 
   (see %s ). 
 
   The quality  of the unvoiced signal components  is further affected
   by the phase randomization flags (see %s).
   
--------------------------------------

-sh_rand <time offset> or <bpf file> ::     %s                (Def.: 0)

   random modulation of frame overlap positions.

   The  time  given  is  interpreted  as  the  limits  of  an  equally
   distributed randomly selected time offset that will be added to the
   optimal  overlap positions of  the subsequent  frames. The  time is
   specified  in ms  and  can be  either  a constant  or  a file  name
   containing a bpf of time offsets for different time positions.  The
   fact that the random variation synchronously shifts the whole frame
   gives an effect of rough sound to the output signal.

   ----- Example : -sh_rand 3
 
   If  applied to  a speech  signal adds  some rough  features  to the
   voice.

---------------------------------------------------------------------
---------------------------------------------------------------------~
   
leccmd:usage-rnd =
=====================================================================
======================  Phase Randomization%s ========================

Phase  randomization is  a  technique  that can  be  used to  randomly
modulate the phase of the spectral representation of the signal. Phase
randomization  is either  used  in shape  invariant  processing (%s) 
to compensate the  loss of randomness of the  noise components if time
stretching is  applied to  the signal. It  can as  well be used  as an
effect to modulate the phase of the different components of the signal
in a random manner. Phase randomization first determines the phase for
perfect signal alignment  with respect to the previous  frame and then
adds  a  random phase  with  equal  distribution  over the  range  +/-
alpha*Pi. Alpha  is the phase randomization level  controlled by means
of user  parameters. The energy loss  that is due  to the cancellation
effects  that   are  related  to   the  random  phase   component  are
compensated. 

The phase randomization module  distinguishes three different types of
signal components and  for each of these components  a different phase
randomization strategy is applied. The first distinguishing feature is
the Voiced/Unvoiced-Frequency  boundary (VUF, see  %s).  

The frequency region above the  VUF is considered to contain noise. In
this  area  the  phase  randomization  is linearly  dependent  on  the
frequency according to the two parameters %s and %s. 

Below the  VUF two different  components may be  distinguished.  These
two are sinusoids  and non sinusoids (noise and  sidelobes).  For both
components an individual constant randomization level can be selected.
Sinusoids will be detected  only, if either shape invariant processing
or sinusoidal remixing (see %s) has been requested.


Phase randomization Parameters are
%s
  -rnd_force  : use randomization as an effect %s

  Randomization above VUF (used for transformations and as effect)
  -rnd_stval  : start value of phase randomization level %s
  -rnd_ff     : freq position of full phase randomization %s
  -rnd_vufke  : VUF handling during transposition %s

  Randomization of sinusoidal components below VUF
  -rnd_sin    : set phase randomization level %s

  Randomization of non-sinusoidal components below VUF
  -rnd_voi    : set phase randomization level %s
%s~

leccmd:usage-rnd_stval =
-------------------------------------

-rnd_stval <level> ::               %s                      (Def.: 0.3)

   specifies the amount of random phase that is added to the synthesis
   phase for phase randomization of noise above VUF.  The value is the
   maximum phase randomization level (see %s) for frequencies 
   right at  the VUF boundary.   The value given  will be used  at the
   voiced/unvoiced  frequency limit  and will  linearly  increase with
   frequency range until  at the frequency specified with  the %s flag
   the   phase   randomization   reaches   its  maximum   with   phase
   randomization level of value 1.

~

leccmd:usage-rnd_ff  =
-------------------------------------
   
-rnd_ff <freq> :: %s                                      (Def.: 22kHz)

   frequency for which maximum randomization of the partial phase will
   be  achieved for  the  signal components  above  VUF. Frequency  is
   specified in kHz

~

leccmd:usage-rnd_force =
-------------------------------------

-rnd_force ::                              %s                (Def. off)

  Enables  use of phase  randomization as  an effect.  Automatic phase
  randomization control in dependency of the transformation parameters
  is switched off.

~
  
leccmd:usage-rnd_vufke =
-------------------------------------

-rnd_vufke <value> ::   %s           (Def. 1 or 0 if -envgl is used %s)

  Selects  the position  of  the VUF  limit  in relation  to a  signal
  transposition.   If  value  is  0   than  the  VUF  used  for  phase
  randomization will  be the original VUF transposed  according to the
  transposition  parameters.  In  the  other case  the  VUF will  stay
  constant relative to the  spectral envelope taking into account only
  transposition specified with -trans/-transnc.
  ~

leccmd:usage-rnd_sin =
-------------------------------------

-rnd_sin <value> :: %s                                       (Def.:  0)

  Randomization level for sinusoidal components below VUF.  Sinusoidal
  detection  is  enabled in  shape  invariant  mode  (see: %s)  or  in
  sinusoidal remixing mode (see %s/-vu_remix). 

  --Example : -rnd_sin 0.2

  Selects phase randomization with +/- 0.2pi for sinusoidal components
  below VUF.

~

leccmd:usage-rnd_voi =
-------------------------------------

-rnd_voi <value> ::                %s                        (Def.:  0)

  Randomization level  for non  sinusoidal components below  VUF.  Non
  sinusoidal components below the VUF can be either sidelobes or noise
  between  the  sinusoids.  In  case  the  VUF  is  set  manually  and
  sinusoidal  detection  is  off  all  components  below  VUF  ar  non
  sinusoidal.  Sinusoidal detection is enabled in shape invariant mode
  (see: %s) or in sinusoidal remixing mode (see %s/-vu_remix).

  --Example : -rnd_voi 0.1

  Selects phase randomization with +/- 0.1pi for non sinusoidal
  components below VUF.

~

leccmd:usage-rnd_nfbp =
-------------------------------------
   
-rnd_nfbp <normalized freq> ::   %s                           (Def.: 1)

   Attention: -rnd_nfbp will soon be discontinued in favor of -rnd_ff.
   Moreover the frequency reference is no longer the Nyquist frequency
   but a fixed value of 22kHz.

   break point  for which maximum  randomization of the  partial phase
   will be achieved in terms of the Nyquist frequency.

---------------------------------------------------------------------~

leccmd:usage-age =

-age <value> ::

  Transforms a voice to make it sound older (with a positive value) or
  younger (with a negative value).

---------------------------------------------------------------------~

leccmd:usage-female =

-female <value> ::

  Transforms a male voice to make it sound like a female voice. Higher
  values will yield a more noticeable change.

---------------------------------------------------------------------~

leccmd:usage-male =

-male <value> ::

  Transforms a female voice to make it sound like a male voice. Higher
  values will yield a more noticeable change.

---------------------------------------------------------------------~


