filtre:badffttype =  
  input FFT type, can't be handled by filter modul
  you probably have requested logarithmic amplitude coding!~
filtre:eof 						 = end of coefficient file `%s' reached at time %f\
filter parameters will be considered constant from now on\n ~

filtre:badtimeorder 			 = the times are not in an increasing order in the filter parameter file~
filtre:badfreqorder 			 = frequencies are not in an increasing order in the filter parameter file~
filtre:nbands 					 = the number of filter bands specified in parameter file must remain constant~
filtre:allocerror 				 = can't allocate memory for a filter module~
filtre:openerror 				 = can't open filter parameter file `%s'\n ~
filtre:paramfile 				 = filter parameter file: %s~
filtre:spectrum 				 = number of spectrum points: %d~
filtre:def 						 = spectral definition: %.2f Hz~
filtre:nophase  = 
Due to ampl/freq coding of spectral data phase part
of the filter function will be ignored for %s-filter
Use a separate run for dilation/cross synthhesis if you need
phase filtering.~

filtre_mode:combine = superimposing filters : using %s mode ~
filtre_mode:applyyes = apply result : yes ~
filtre_mode:applyno = apply result : no ~

filtre-arma:moduletype 			 = \nArma filter module~
filtre-arma:allocerror 		 = can't allocate memory in arma filter~

filtre-bande:moduletype 		 = \nBand filter module~
filtre-bande:allocerror 		 = can't allocate memory in band filter~
filtre:fileempty 			 = filter parameter file is empty~
filtre-bande:fileformaterror 		 = error reading band filter parameter file %s at pos %d ~
filtre-bande:begin 				 = begin values are different in band filter file~
filtre-bande:bandnumchanged 		 = Change in number of filter bands not supported for band filter !\n ~
filtre-bande:nonpositive 		 = number of filter bands in parameter file is less than one~
filtre-bande:noneven 		 = number of filter bands in parameter file has to be even, specify start and end of each band~
filtre-bande:nbbtoobig 			 = number of filter bands in parameter file > FFT size~
filtre-bande:badstartval 		 = the state of the first band must be either 0 or 1 in the filter parameter file~
filtre-para:extrapolate                 = extrapolation : %s ~
filtre-para:scalemean                   = scale mean    : %s ~
filtre-para:transparent                 = transparency  : %s ~

filtre-breakpt:allocerror 		 = can't allocate memory in breakpt filter~
filtre-breakpt:nopoints 		 = negative or zero number of filter points specified in breakpt file~
filtre-breakpt:moduletype 		 = \nBreakpoint filter module~
filtre-breakpt:fileformat 		 = unknown mode encountered in breakpt file~
filtre-breakpt:fileformaterror 		 = breakpt filter parameter file %s has wrong format (readpos = %d). \n~
filtre-breakpt:fileformaterrormc 		 = breakpt filter parameter file %s has wrong format. Mode changed before file pos = %d \n~
filtre-breakpt:freqlimit 		 = frequency %.2f Hz is greater than Nyquist limit in breakpt parameter file\
\tcorrecting to Nyquist limit %.2f Hz\n ~


filtre-fifof:moduletype 		 = \nFifof (spectral interpolation) filter module~
filtre-fifof:allocerror 			 = error allocating memory for fifof filter~

filtre-fof:moduletype 			 = \nFof (formant parameter interpolation) filter module~
filtre-fof:allocerror 			 = error allocating memory for fof filter~
filtre-fof:nformants 			 = number of formants: %d~

filtre-fof:fileempty 			 = fof filter parameter file is empty~
filtre-fof:onedataset 			 = there's only one data set in the fof file~
filtre-fof:fileformaterror 		 = error reading fof filter parameter file %s at pos %d ~
filtre-gabarit:allocerror 		 = can't allocate memory for gabarit filter~
filtre-gabarit:moduletype 		 = \nGabarit filter module~
filtre-gabarit:fileheaderr 		 = Error reading header for filter file %s~
filtre-gabarit:fileempty 		 = filter parameter file is empty~
filtre-gabarit:onedataset 		 = there's only one data set in the gabarit file~
filtre-gabarit:badformat 		 = filter mode in gabarit filter file has to be mode 1 (amplitude) or 2 (phase) or 3 (amplitude/phase)~
filtre-gabarit:unsupportedsdiffile = sdif file %s does not contain valid info frames of type IAVS or IGBG which are needed to provide global filter infos to gabarit filter module.\n~
filtre-gabarit:incompleteframe = error reading gabarit filter frame from file %s\n~
filtre-gabarit:reechant  = Internal error in the filter description modul. \nPlease
this error with parameter settings to IRCAM.~

filtre-gabarit:mod                       = gabrit file mode : %d (%s)~
filtre-gabarit:pas                       = filter freq res  : %f ~
filtre-gabarit:descrint  = filter parameter file interpolated due to analysis parameter mismatch!~
filtre-gabarit:descrcp  =  filter file and analysis parameters match!~


filtre-subtract:allocerror 		 = can't allocate memory for sub filter~
filtre-subtract:moduletype 		 = \nSubtract filter module~
filtre-subtract:fileheaderr 		 = Error reading header for filter file %s~
filtre-subtract:fileempty 		 = filter parameter file %s is empty~
filtre-subtract:onedataset 		 = there's only one data set in the gabarit file~
filtre-subtract:badformat 		 = filter mode in gabarit filter file has to be mode 1 (amplitude)~
filtre-subtract:reechant  = Internal error in the filter description modul. \nPlease
this error with parameter settings to IRCAM.~
filtre-subtract:noigbfmatrixinframe    = file %s bad sdif 1AVS frame -> does not contain IGBF matrix ~
filtre-subtract:nosdiffile  = error reading %s\nspectral subtraction module requires sdif parameter file~
filtre-subtract:no1avsmatrixinframe    = file %s bad sdif 1AVS frame -> does not contain 1AVS matrix ~
filtre-subtract:alpha                 = trans. width (alpha) : %s ~
filtre-subtract:beta                  = max noise atten. (beta) : %s (dB)~
filtre-subtract:relax                 = relaxation const : %s ms~
filtre-subtract:subdetyes             = spec treat. sinusoids : yes ~
filtre-subtract:subdetno              = spec treat. sinusoids : no ~
filtre-subtract:factor                = std. margin (filterfac) : %s ~
filtre-subtract:pas                   = filter freq res  : %f ~
filtre-subtract:descrint          = filter parameter file interpolated due to analysis parameter mismatch!~
filtre-subtract:descrcp           = filter file and analysis parameters match!~
filtre-subtract:badstreamsel      = can not handle stream selections with more than a single stream in %s ~

filtre-harmony:moduletype 		 = \nHarmony filter module~
filtre-harmony:allocerror 		 = can't allocate memory for harmony filter~
filtre-harmony:badformat 		 = unknown filter mode in harmony filter file~

filtre-surface:allocerror 		 = can't allocate memory for surface filter~
filtre-surface:moduletype 		 = \nSurface filter module~
filtre-surface:badparameterfile = error reading surface parameter file %s ~

filtre-compressed-gab:moduletype = \nCompressed gabarit filter module~
filtre-compressed-gab:allocerror = can't allocate memory for compressed-gabarit filter~
 
filtre-clip:moduletype = \nClipping filter module ~
filtre-clip:normalization   = re-normalization : %s ~
filtre-clip:region   = ---  Region %d ~
filtre-clip:startend   = Between times        : %.2f %.2f ~
filtre-clip:low   = low amp threshold (dB)     : %f ~
filtre-clip:high = high amp threshold (dB)     : %f ~
filtre-clip:scanfileerr = Bad format reading clipping filter parameter file\n~
filtre-clip:allocerror 		 = can't allocate memory in clipping filter~

filtre-denois:moduletype = \nDenoise surface filter module ~
filtre-denois:allocerror = can't allocate memory for denoise filter~
filtre-denois:badparameterfile = error reading denoise parameter file %s ~

filtre:fshiftnophase = Frequency shifting is not possible together with Amplitude/Frequency encoding !!~
filtre-fshift:fileempty = fshift parameter file empty!!~
filtre-fshift:fileformaterro = error reading fshift parameter %s pos %d!!~
filtre-fshift:moduletype     = \nFrequency Shift module ~
filtre-fshift:allocerror     = can't allocate memory for fshift filter~
filtre-fshift:moduloverflow  = filtre-fshift internal error: modulation buffer overflow  at startpos %d\n~
filtre-fshift:forwardonly    = filtre-fshift error: newstartpos (%d) < old startpos (%d) !!\n~ 
filtre-fshift:shiftparam     = frequency shift : %s Hz ~
filtre-fshift:shiftparam_bpf = frequency shift : %.1f - %.1f Hz ~

filtre-envscal:allocerror = can't allocate memory for envscal filter!~
filtre-envscal:moduletype = \nEnvelope scaling filter module~
filtre-envscal:esfilereadfallback = warning reading file %s : cannot find envelope scaling bpf for channel %d, use channel 0~
filtre-envscal:esfilereaderror  = error reading file %s : cannot find envelope scaling bpf!~
filtre-envscal:envsbpfname    = envscale bpf  : %s ~
filtre-envscal:mlevsbpfname   = mean level scale bpf  : %s ~
