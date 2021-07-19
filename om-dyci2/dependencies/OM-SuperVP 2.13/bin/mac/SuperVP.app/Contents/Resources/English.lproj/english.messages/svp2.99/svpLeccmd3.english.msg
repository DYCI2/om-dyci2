leccmd:preserveenvinconsistent = sorry, can not figure out the type of envelope preservation you selected with argument %s ~
leccmd:inplaceandoutputfile = sorry, inplace processing can not be selected if
outputfile (%s) has been specified ~
leccmd:gabmodeerror = invalid unified file format selected <%s> \nvalid formats are in the range 0-%d\n~
leccmd:dataoutputsndrequest  = Output options inconsistent with data type.\nYou cannot create a snd file from spectral data in the output module!\n~ 
leccmd:sndoutputdatarequest  = Output options inconsistent with data type.\nYou cannot create a unified file format from snd data in the output module!\n~ 
leccmd:unknowndataformat  = unknown output format requested: %s \n~ 
leccmd:Cflagwithoutnumber = -C given without number (>0) specifying the selected channel~
leccmd:cflagwithoutnumber = -c given without number (>0) specifying the selected channel~
leccmd:parametershape     = shape argument (%d) is not a power of 2!~
leccmd:parametermissing   = required parameter missing for flag %s ~
leccmd:parameterranged   =  parameter range [%d,%d] exceeded  for flag %s (=%d)~
leccmd:parameterrangef   =  parameter range [%f,%f] exceeded  for flag %s (=%f)~
leccmd:parameter_tdband = parameter -td_band out of range [%f,%f]~
leccmd:logfile   = no logfile specified after parameter -logfile~
leccmd:synthesimp 			= Can't synthesize with this analysis type~
leccmd:notnumber 			= expecting number in command line argument %s ~
leccmd:unexpectednumber			= unexpected number %s after  command line argument ~
leccmd:notstring 			= expecting a filename for command line argument~
leccmd:zshift1 				= window step (-I flag) is zero~
leccmd:zshift2 				= window step (-i flag) is zero~
leccmd:shift1 				= file name missing after -I flag~
leccmd:shift2 				= file name missing after -i flag~
leccmd:window1 				= you have to specify a mode (num, dep, sync) with -M~
leccmd:window2 				= you have to specify a mode (num, dep, sync) with -m~
leccmd:badFFTtype1 			= bad FFT format in track one (-P option)~
leccmd:badFFTtype2 			= bad FFT format in track two (-p option)~
leccmd:needfilterfile1 		= require a filename after channel one filter type (-F)~
leccmd:needfilterfile2 		= require a filename after channel two filter type (-f)~
leccmd:wrappinglimit 		= wrapping limit~
leccmd:flagP 			= flag p is obsolet and no longer supported
phase synchronized dilation is only possible in the first channel using -P ~
leccmd:outputflagskipped        = ":" flags or parameter file after -O option skipped. These parameters are only available for output of analysis data!~
leccmd:unknownflag 		= unknown flag~
leccmd:nnFlag = !! ATTENTION!!: Output normalization is switched OFF by default since SuperVP 1.75!! Support for the -nn flag will be discontinued in the future. See -norm flag to switch normalization on and select a normalization level.~
leccmd:outputfilepos 		= fatal error: unexpected parameter %s! Output filename has to be last parameter\n ~
leccmd:analysedatafromaudio = Analysis output options can not be given together with synthesis or resampling flag!~
leccmd:badshifttype1 		= you have to specify a mode (num, dep, sync, pos) with -I~
leccmd:badshifttype2 		= you have to specify a mode (num, dep, sync, pos) with -i~
leccmd:callbackiowfile = You can not supply an input file with callback based IO~
leccmd:badwindowtype1 		= bad window length type in track one~
leccmd:badwindowtype2 		= bad window length type in track two~
leccmd:shift>windowsize = Window step (-I) of %.2f samples times time stretch factor (%.2f) is larger than the window size (-M) %d samples. Use automatic window step (remove the -I option) or reduce the window step to %.2f samples.~
leccmd:shifts>windowsizes = Window step (-I) of %.2fms times time stretch factor (%.2f) is larger than the window size (-M) %.2fms. Use automatic window step (remove the -I option) or reduce the window step to %.2fms.~
leccmd:inconsistent_pars        = Inconsistent parameter specification. Step and window size have to be specified either both in samples or both in time units.~
leccmd:inconsistent_pars2       = Inconsistent parameter specification. Step and window size for both tracks have to be specified either all in samples or all in time units.~
leccmd:noanalysis1 			= No analysis will be done on track one (-A option missing)\
\tI hope you're using analysis data as input!  Otherwise, my calculations\
\twon't really make much sense.\n ~

leccmd:synthfromsnd1 = Synthesis from sound data requested in track one: use either spectral data as input (see -isdata flag) or add analysis flag (see -A)~ 
leccmd:synthfromsnd2 = Synthesis from sound data requested in track two: use either spectral data as input (see -isdata flag) or add analysis flag (see -A)~ 
leccmd:mixsndandanalysis = Cannot mix snd and spectral data in track one and two~
leccmd:isdataerror = No track specified for isdata flag.~
leccmd:analysiswithdataU = Analysis selected for data input in track one~
leccmd:analysiswithdataD = Analysis selected for data input in track two~
leccmd:noanalysis2 			= No analysis will be done on track two (-a option missing)\
\tI hope you're using analysis data as input!  Otherwise, my calculations won't really make much sense.\n ~
leccmd:nosynthesis 			= No synthesis will be done (-Z option missing): I hope you wanted analysis data as output! \n ~

leccmd:noHwithtransposition = -H are exclusive options and can not be used together ~
leccmd:nodilationwithanalysisdata = cannot use dilation factor without FFT  analysis \n ~
leccmd:baddilate 			= Dilation coefficient is negative or null.~
leccmd:fixstep 				= as a result of dilation, analysis increment has been changed to %f\n\n~
leccmd:Ntoosmall                        = FFT size (%d) of track 2 too small to process window of size %d samples;~
leccmd:Mtoobig 				= The size of the analysis window M (%d) is greater than the fftsize (%d). \
\tChanging fftsize into : %d.\n ~


leccmd:nowindowsizet2                   = you have to specify the size of the analysis window for track 2.~
leccmd:Ltoobig 				= The size of the synthesis window L is greater than N. \
\tChanging L into : %d.\n  ~

leccmd:MsmallerthanL 		= The size of the analysis window M is greater than the size of the synthesis window L. \
\tYou may obtain possible audio distorsions in the result.\n ~

leccmd:badfixstep 			= the parameters have produced a step size of zero! ~

leccmd:nchannels_different 	= the two tracks have different FFT sizes %d and %d.\n~
leccmd:windowsize_different = The window sizes for the two tracks don't match. ~
leccmd:vuv_sync 			= I can't perform a "vuv" analysis unless channel type is "sync". ~

leccmd:unknow_mixer                     = parameter error: unknonw mixer module <%s> provided with -G flag ~
leccmd:mixmullpc 			= mixer mode 'mul' will use FFT analysis on the primary channel and\
\tLPC analysis on the secondary channel (DEFAULT).  Your analysis-type request(s)\
\t(-Aa options) will be ignored if different of LPC,CED,LPC_INV, or CED_INV.~

leccmd:mixcrosslpc 			= mixer mode 'cross' will use FFT analysis on the primary\
\tchannel.  Your request (-A option) is being ignored.~

leccmd:mixcrosslpc2 		= since LPC analysis has been requested on the secondary\
\tchannel in mixer mode 'cross', the coefficients "x" and "y" will be zero ~

leccmd:mixcrosslpc3 		= analysis mode (-a option) must be FFT or LPC for mixer mode 'cross'~

leccmd:mixaddlpc 			= mixer mode 'add' will use FFT analysis on the primary\
\tand secondary channels.  Your analysis-type request(s)\
\t(-Aa options) are being ignored.~
leccmd:mixaddfromfile  = error reading mix parameter file %s : -Gmul does not support any mixing parameters ~
leccmd:wrongfftsize = a number is missing for fftsize specification in %s\n~
leccmd:fromstdin 			= no input sound file is specified (-S, -s options)~
leccmd:nolpc 				= LPC analysis isn't allowed on a single channel, changing to FFT~
leccmd:nomask 				= Masking analysis isn't allowed on a single channel, changing to FFT~
leccmd:nomaskch2 			= mask flag on track2 is not allowed!\n~
leccmd:badmixtype 			= mixer type (-G option) must be either mul, cross, or add~
leccmd:cannotmix 			= you must specify a second channel (-s) when mixing (-G)~
leccmd:nophasesyncwithmixcross = Phase synchronization switched off because of requested cross synthesis of type cross! ~
leccmd:dilimpossible 		= (-Z) resynthesis must be used if dilating or\
\ttime correcting a transposition (-D)\n ~

leccmd:normalistionexpected = A mask analysis MUST be normalised (-ns)\
\tNow, spectral normalisation is set. ~

leccmd:gabaritformaterror 	= Output format for gabarit file is set by the\
\tgabarit mode - SuperVP won't take care of -Ogx: format options. ~

leccmd:nbsamples 			= raw sound is Piped : specifie sound length with -E \n ~
leccmd:whichsamplerate 		= raw sound is Piped : specifie sample rate for the synthesis with -R.\n ~
leccmd:FileFormatUnknown 	= File Format %s unknown. Using AIFF Format. \n ~
leccmd:SampleSizeUnknown 	= Sample size %s unknown for aiff/aifc format. Using default sample size of %d bits. \n ~
leccmd:FilePackUnknown 		= File packing %c unknown for %s file format. Using %s file packing. \n ~
leccmd:nooutfromchannel2	= -o not supported\nno output possible from channel 2~
leccmd:reswindow = with the new FIR filter design implemented for\ 
       the interpolation filter the -resW parameter is no longer supported.\
       The window to be used is always a kaiser window. To adjust window\
       size use the -resS parameter !~
leccmd:fold = Since SuperVP 1.83 the -fold option is no longer needed.\n~
leccmd:avsegfileinpair = invalid average segments file %s.\n There need to be an even number of times specifying the start and end poiunts of segments in this file!!\n~
leccmd:badmasktype =  Invaild mask type specified for -Amask flag!\n~
leccmd:envgl_not_used  = Ignoring -envgl parameters for track 1 because no fundamental frequency (-F0)  has been specified !\n~
leccmd:envgl_not_used2  = Ignoring -envgl parameters for track 2 because no fundamental frequency (-f0)  has been specified !\n~
leccmd:mdl-invalid = Invalid values for %s\n~
leccmd:mdl-noamp = Missing amplitude for %s\n~
leccmd:badtransenv = Invalid parameter for %s: must not be negative.~
leccmd:baddupspec = Invalid parameter for -dup: %s\n~
leccmd:keepchannelana = Keeping unprocessed channels is only supported with audio input and output, 'k' modifier ignored\n~
leccmd:Cflagana = Channel selection -C is only supported with audio input\n~
leccmd:cflagana = Channel selection -c is only supported with audio input\n~
leccmd:framenoaverage = average spectra not supported in S7 format\n~
