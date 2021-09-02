ica-data:noselection  = specification of start/end time not possible for processing of analysis data\n~
ica-data:openerror 			= \ncan't find analysis data file `%s' \n ~
ica-data:open_stdin_error 	= \n Error opening sdif file from stdin. Reading gabarit format from stdin is no longer supported!!\n ~
ica-data:allocerror 		= can't allocate memory for a new Canal_Data object~
ica-data:allocerror2 		= can't allocate memory for Canal_Data data~
ica-datamem:moduletype 		= \nAnalysis data callback input channel~
ica-data:moduletype 		= \nAnalysis data input channel~
ica-data:fileerror 		= \nError Reading Data File %s! ~
ica-data:formaterror 		= \nUnsupported data format only 1GB3, 1GB4, 1GB5 and 1GB7 can be used ~
ica-data:noframes           = Error Reading Data File %s : frame missing \n~
ica-data:nosync             = Error Reading Data File %s : All frames have to be synchronized \n~
ica-data:unsupportedsdiffile = Error Reading Data File %s : No supported data frames found.\n~
ica-data:fftsizeneqparafile = \n\nFFT size wanted (N=%d) differs from FFT size of the input data file (N=%d). Using N=%d.\n ~
ica-data:fftsizeinconsistent = \n\nFFT size in different channels of input data file differs previous value %d  current value %d.\n ~
ica-data:Ineqparafile       = \n\nI step wanted (I=%f) differs from I step of the input data file (I=%f). Using I=%f.\n ~
ica-data:Mneqparafile       = \n\nM size wanted (M=%d) differs from M size of the input data file (M=%d). Using M=%d.\n ~
ica-data:notyet 	= \n Sorry, the  %s encoding cannot yet be obtained from analysis files in %s format.\n~
ica-data:nchans 			= number of channels: %d~
ica-data:fileheadersflentobig = Name of snd input file in file header of %s
exeeds %d bytes. This appears to be an invalid gabarit file!\n~
ica-sync:moduletype 		= \nInput channel : synchronized~
ica-sync:filename 			= filename: %s~
ica-sync:shift 				= shift coefficient: %f~
ica-sync:length 			= length coefficient: %f~
ica-sync:eof 				= reached end of synchronization file~

ica:buffersize 				= window length %d is greater than maximum allowed value %d\n ~
ica:openerror 				= parameter file open error: %s\n ~
ica:onechannel  = only one channel allowed in gabarit file %s \n ~
ica:length 				= window size parameter file: %s~
ica:shift 				= increment parameter file: %s~
ica:pitchshift 				= pitch (increment) parameter file: %s~
ica:pitchlength 			= pitch (length) parameter file: %s~
ica:position 				= window pos. parameter file: %s~
ica:noteven 				= analysis window size %d isn't even - increasing to %d\n ~
ica:datafilename 			= data filename: %s~
ica:increment 				= analysis increment: %f~
ica:track                               = track             : %d ~
ica:M 					= analysis window size: %d~
ica:Mminmax    			        = analysis window size: %d - %d~

ica:deplzero = Analysis window increment equal to zero\n        Check -I option or time stretching parameters!\n~

ica:deplsmall = Analysis window increment is 1 sample and more
then 10000 steps are required to process the sound!\
      If you intentionally used this parameters lean back this may take some time.
      Otherwise: check -I option or time stretching parameters!~

ica-dep-dep:moduletype 		= \nInput channel with time-variable window length and shift~

ica-dep-pos:moduletype 		= \nInput channel with time variable window length and external position~

ica-dep-num:moduletype 		= \nInput channel with time-variable window length~

ica-dep-sync:moduletype 	= \nInput channel with time-variable window length and pitch-synchronous shift~

ica-num-dep:moduletype 		= \nInput channel with time-variable shift~

ica-num-num:moduletype 		= \nInput channel (Master)~
ica-num-num:moduletype-slave 	= \nInput channel (Modulator)~
ica-num-mem:moduletype 		= \nInput channel memory io (Master)~
ica-num-mem:moduletype-slave 	= \nInput channel memory io (Modulator)~
ica-num-num:increment 		= analysis increment: %f~
ica-num-mem:increment-auto 	= analysis increment: %f (adaptive)~
ica-num-num:incrementlimits 	= variable increment: [%f - %f]~
ica-num-mem:oversampling_ana    = min oversampling (ana): %f~
ica-num-mem:oversampling_synth  = min oversampling (syn): %f~
ica-num-mem:oversampling_synth_skip = min oversampling (skip syn): %f~
ica-num-num:M 			= analysis window size: %d~
ica-num-num:autodeplacementnotsupp = you need to specify the increment using -i for track 1 if track 0 duration is not accessible.\n~

ica-num-mem:monoonly            = memory io does not work with multi channel files yet!\n~
ica-num-pos:fileempty 		= position file is empty~
ica-num-pos:moduletype 		= \nInput channel with external position (Master)~
ica-num-pos:moduletype-slave	= \nInput channel with external position (Modulator)~
ica-num-pos:posfile-read        = Error reading position file %s: line <%s> ~
ica-num-pos:posfile-empty       = position file %s is empty~
ica-num-sync:moduletype 	= \nInput channel with synchronous shift~

ica-spector:allocerror 		= can't allocate memory for Canal_Spector~
ica-spector:allocerror2 	= can't allocate memory for Canal_Spector data~
ica-spector:moduletype 		= ica-spector input module~
ica-spector:params 			= \tM = %d, N = %d, I = %d R = %f base = %d\n ~

ica-sync-dep:moduletype 	= \nInput channel with synchronous window length and time-variable shift~

ica-sync-num:moduletype 	= \nInput channel with time-variable window length~

ica-sync-pos:moduletype 	= \nInput channel with time-variable window length and external position~

ica-sync-sync:moduletype 	= \nInput channel with synchronous window length and time-variable shift~

ica-reverse:moduletype 		= \nInput channel : reverse~

icanal:badautoM                 = bad option in windowsize from F0 parameter string %s ~
icanal:badautoMPar              = unknown option %s in windowsize from F0 parameter string ~
icanal:f0file                   = f0 file or value : %s ~
icanal:vuffile                  = vuf file or value : %s ~
icanal:f0_channel_to_stream     = f0 data read from stream : %d ~
icanal:vuf_channel_to_stream    = vuf data read from stream : %d ~
icanal:mixfile 			= mixin  from file : %s~
icanal:f0filereadfallback       = warning reading file %s : cannot find F0data for channel %d, use channel 0~
icanal:f0filereaderror          = warning reading file %s : cannot find F0data!~
icanal:vuffilereadfallback      = warning file %s :cannot find VUF data for channel %d, use channel 0~
icanal:vuffilereaderror          = warning reading file %s : cannot find VUFdata!~
icanal:inplacewithchannelselect = Inplace processing requires same number
of channels in input and output stream. See help on  keeping all channels
modifier for channel selection in input options! ~
icanal:inplacenoseek            = inplace processing requested for non seekable file %s!\n~
icanal:monostereocross 		= can't mix a mono sound with a stereo sound~
icanal:Mconstant 			= end of variable window size parameter file at time %f\
\tparameters will be considered constant from now on\n ~

icanal:Iconstant 			= end of variable analysis increment parameter file at time %f\
\tparameters will be considered constant from now on\n ~

icanal:posEOF 				= end of input channel position file~
icanal:Mempty 				= variable window size parameter file is empty~
icanal:Iempty 				= variable analysis increment parameter file is empty~
icanal:allocerror 			= can't allocate memory for a new channel module~
icanal:allocerror2 			= can't allocate memory for channel module data~
icanal:allocerror3 			= can't allocate memory for stereo channel data~
icanal:beginafterend 		= channel end time is before begin time~
icanal:begintoobig 			= can't begin where you ask, the file is too short\n~
icanal:chanexceed                       = Selected channel (%d) exceeds number of channels (%d) in file %s!\n~
icanal:endtoobig 			= The end time is past the end of file. \
\tcorrecting by setting the end time to the size of the file.~

lib-sf:inplacenonseek = Cannot work inplace on pipe %s !!\n~
icanal:fseek 	      = seek error on input file %s!\n~
icanal:notinitialized 		= can't allocate memory for uninitialized channel~
icanal:unknownfileformat 	= sound file `%s' is in an unknown format\n~
icanal:readerror 			= error in reading sound file `%s'\n ~
icanal:stdincseek 			= cseek can't be used on stdin~
icanal:seeknegative 		= can't cseek to a negative value (before sound starts)~
icanal:beyondsignal 		= I can't go beyond the end of the signal~
icanal:lockedchannel 		= can't set analysis increment because channel is locked~
icanal:buffersize 			= buffer size: %d~
icanal:soundname 			= sound file name: %s~
icanal:soundpattern 			= sound file pattern: %s~
icanal:format 				= \n\tformat\n ~
icanal:samplerate 			= sampling rate: %.2f KHz~
icanal:processchan                      = process channel: %d ~ 
icanal:processall                       = process channel: all ~ 
icanal:keepmultitrue                    = keep multi channel: yes ~
icanal:keepmultifalse                   = keep multi channel: no ~
icanal:duration_int 			= duration of the file: %0+12.6f seconds (%0+10ld sample frames) ~
icanal:eduration_int			= effective duration: %0+12.6f seconds (%0+10ld sample frames) ~

icanal:start_int  			= start: %0+12.6f seconds (%0+10ld sample frames) ~
icanal:end_int  			= end: %0+12.6f seconds (%0+10ld sample frames) ~
icanal:duration_unknown 		= duration of the file: unknown ~
icanal:duration 			= duration of the file: %0+12.6f seconds (%0+12lld sample frames) ~
icanal:eduration 			= effective duration: %0+12.6f seconds (%0+12lld sample frames) ~

icanal:start 				= start: %0+12.6f seconds (%0+12lld sample frames) ~
icanal:end 				= end: %0+12.6f seconds (%0+12lld sample frames) ~
icanal:format 				= \tformat~

icanal:duration 			= duration: %f %s~
icanal:seconds 				= seconds~
icanal:bufferoverrun  = icanal buffer overrun in positionne. Please report this situation with the parameter settings of SuperVP as a bug to IRCAM~

icanal:noinplacewithcompression   = Inplace processing is not possible for compressed input sound files!~
icanal:noinplacewithmultichan = Inplace processing is only possible for mono input sound files!~
icanal:errorgetsig         = Error reading samples from file "%s" \n~
icanal:mixfilereaderror  = error reading mix file : line <%s> ~
icanal:mixfileopenerror  = can't open mixer input sound file `%s' \n ~
icanal:mixfilechannelrange   =  mix file error: target channel (=%d) and source channel (=%d) have to be >0 \n ~
ica-dataspec:moduletype         = \nSpectral data callback input channel~
ica-modulate:pitch   = Pitch~
ica-modulate:env     = Envelope~
ica-modulate:freq    = freq~
ica-modulate:amp     = amp~
ica-modulate:fixed-env-amp     = Envelope modulation amp: %f~
ica-modulate:fixed-env-freq    = Envelope modulation freq: %f~
ica-modulate:fixed-pitch-amp   = Pitch modulation amp: %f~
ica-modulate:fixed-pitch-freq  = Pitch modulation freq: %f~
ica-modulate:random-env-amp    = Envelope modulation amp: from %f to %f, change every %f seconds~
ica-modulate:random-env-freq   = Envelope modulation freq: from %f to %f, change every %f seconds~
ica-modulate:random-pitch-amp  = Pitch modulation amp: from %f to %f, change every %f seconds~
ica-modulate:random-pitch-freq = Pitch modulation freq: from %f to %f, change every %f seconds~
ica-dataframe:moduletype        = \nAnalysis data callback input channel~
