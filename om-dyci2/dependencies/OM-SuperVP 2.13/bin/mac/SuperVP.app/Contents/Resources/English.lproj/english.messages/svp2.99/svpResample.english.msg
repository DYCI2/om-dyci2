resampler:connectformaterror = Resampler requires temporal input data!! Check your parameter flags !!~
resampler:moduletype 	= \nTemporal resampling module~
resampler:paramfile 	= transposition param file: %s~
resampler:oldrate 		= original sampling rate: %.2f KHz~
resampler:newrate 		= new sampling rate: %.2f KHz~
resampler:changepitchnc	= transpose (no time corr) by: %.1f cents~
resampler:changepitch 	= transpose by: %.1f cents~
resampler:changepitch2 	= transpose in the range between: %.1f - %.1f cents~
resampler:changepitch2nc = transpose in the range between: %.1f - %.1f cents~
resampler:windowsize 	= convolution window size: %d~
resampler:stopatt 	 = interp. filt stop band att : %f~
resampler:filterover 	 = oversampling factor : %d~
resampler:fileempty 	= resampler parameter file is empty~
resampler:eof 	        = end of coefficient file `%s' reached at time %f\
the resampling parameter will be considered constant from now on\n ~
resampler:convalloc     = Unsufficient memory in setup_convolution_kernels !~
resampler:outstorealloc = Unsufficient memory for allocation of segement store in resampler!~
resampler:openerror 	= can't open transposition parameter file `%s'\n ~
resampler:parameter 	= "-trans" should be followed by a blank space and either\
\tthe number of cents to transpose, or a filename with time-varying cents\n ~
resampler:cacheoverflow   = cachesize to small to hold incoming data in resampling modul\n This is a bug please report together with your program parameter settings~ 
resampler:cacheoverflowatend   = cachesize to small to hold extension of input data in resampling modul\n This is a bug please report together with your program parameter settings~ 
resampler:overrun       = output segment skipped ! report this bug
together with SuperVP parameters to IRCAM!~
resampler:outStoreoverflow = resampler outStore overflow ! Report this bug
together with SuperVP parameters to IRCAM!~
resampler:bufsize = resampler output connection overrun. Report this bug
together with SuperVP parameters to axel.roebel@ircam.fr !~
resampler:cacheoverflow = resampler cache overflow overflow ! Report this bug
together with SuperVP parameters to IRCAM!~
