ocanal:allocerror 			= can't allocate memory for a new ocanal module~
ocanal:openerror 			= error opening output file `%s'\n ~
ocanal:filename 			= file name: %s~

ocanal-data:moduletype 		= \nAnalysis data output module~
ocanal-data:parserror 		= Parsing error in output module. Missing " ?~
ocanal-data:allocerror 		= can't allocate memory for new ocanal-data module~
ocanal-data:iffterror           = error performing inverse fft in ocanal-data!\n~
ocanal-data:ascii     		= ASCII~
ocanal-data:binary    		= binary~
ocanal-data:gabarit   		= Unified File Format~
ocanal-data:sdif   		= SDIF File Format~
ocanal-data:format 		= format: %s~
ocanal-data:gabmode 		= mode: %d~
ocanal-data:avmode 		= average mode: %d~
ocanal-data:avsegs              = segment %3d : [ %f, %f ]~
ocanal-data:indexMin 		= index min: %d~
ocanal-data:indexMax 		= index max: %d~
ocanal-data:nosdifsupport       = no sdif support for selected output in file `%s'\n ~
ocanal-data:nofile          = no output file available to write data\n ~

ocanal-data:extratime       = : time~
ocanal-data:extratpn        = : number of channels~
ocanal-data:extraformat     = : data format~
ocanal-data:extra 	    = window header:~
ocanal-data:srate 	    = input sampling rate: %.2f KHz~

ocanal-data:column1         = column1~
ocanal-data:column2         = column2~
ocanal-data:bothcolumns 	= columns 1 and 2~
ocanal-data:datawritten     = data written: %s~
ocanal-data:missingargs 	= Missing arguments (did you put a space character ?)~
ocanal-data:wrongpar 		= can't calculate parameter "%s" with this analysis~
ocanal-data:unknownpar 		= Unknown parameter "%s" in data ouput module~
ocanal-data:openerror 		= Can't open file : %s for data output module~
ocanal-data:badformat 		= Bad format in format description file~
ocanal-data:invalidpar 		= Invalid parameter number %d in format description file~
ocanal-data:ascii 			= ASCII format~
ocanal-data:binary 			= Binary format~
ocanal-data:formaterror 	= Format error in format description file. Using default~
ocanal-data:numberargs 		= Number of arguments doesn't match for output. Using default format~
ocanal-data:ampfreqnotsupported = no data output supported for Amplitude/Frequency encoding!~ 
ocanal-data:badffttype          = unknown data coding in ocanal-data.~
ocanal-data:av_invalidsegmentinfo = invalid segment boundaries (%f,%f) read from %s:\n first bound  has to be below second!~ 
ocanal-data:averagedatamissing   = no signal frames contained in segment: \neither increase your segment sizes or reduce analysis step "-I"~ 


oca-data-vuf:moduletype   = \nVoiced/Unvoiced frequency limit output module~

oca-data-transient:moduletype   = \nTransient data output module~
oca-data-transient:transient-mode-trans = transient mode : mean time ~
oca-data-transient:transient-mode-trans-extended = transient mode : mean time (extended output)~
oca-data-transient:transient-mode-specdif = transient mode : spectral distance ~
oca-data-transient:badffttype   = unsupported fft coding detected in run_oca_data_transient!\n This is a bug. Please send this info with your parameters to roebel@ircamfr!\n~
ocanal-data:envmode             = data : envelope + coefficients ~
ocanal-data:noinplace           = Cannot create data output in inplace mode\n~
ocanal-son:openerror            = error creating sndfile `%s'\n~
ocanal-son:level                = at minimum play buffer contained %f sec.\n~
ocanal-son:errorplaynodev       = Error initializing audio device:: no device available.\n~
ocanal-son:errorplayinit        = Error initializing audio device::error message return <%s> .\n~
ocanal-son:errorplay            = Error handling audio <%s>.\n~
ocanal-son:userplaybufsizeign   = Ignoring user supplied play buffer size = %f < 0.01sec, \n -> using minimum value = 0.01sec~
ocanal-son:useshort             = unsupported integer size requested: useing short!~
ocanal-son:usefloat             = unsupported float size requested: useing float!~
ocanal-son:inplaceopenerror 	= error opening sound file `%s' \n ~
ocanal-son:overflow             = \nWarning:: Overflow of valid sample range detected in output channel %d, output will be clipped\ntry with normalization!\n\n~
ocanal-son:overflowfloat        = \nWarning:: Overflow of standard sample range detected in output channel %d, output may be not correctly handled by all applications in this case try with normalization!\n\n~
ocanal-son:moduletype 		= \nSound output module~
ocanal-son:allocerror 		= can't allocate memory for new ocanal-son data~
ocanal-son:badfileformat 	= unknown sound file format~
ocanal-son:writeerror 		= error writing data file %s ~
ocanal-son:diskfull 		= DISK FULL! \n error writing data file %s.~
ocanal-son:soundheader 		= error in writing sound header~
ocanal-son:tempopenerror 	= error in opening temporary output file~
ocanal-son:tempopenerror2 	= error in opening temporary output file: system message was <%s> !~
ocanal-son:unknownformat 	= unknown file format, so I'll try AIFF~

ocanal-son:carlstandard     = carl standard~
ocanal-son:carlircam        = carl Ircam~
ocanal-son:aiffaifc         = MacIntosh,SGI AIFF/AIFC~
ocanal-son:aifc             = MacIntosh,SGI AIFC~
ocanal-son:aiff             = MacIntosh,SGI AIFF~
ocanal-son:next             = NeXT~
ocanal-son:sdii             = Sound Designer II~
ocanal-son:wav              = WAV~
ocanal-son:raw              = raw (no header)~
ocanal-son:unknown          = unknown~
ocanal-son:soundformat 		= sound file type: %s~
ocanal-son:playunderrun  = Warning: bufferunderrun in playbuffer you
may better increase the play buffer~
ocanal-son:playing 		= output : direct play ~
ocanal-son:playbufsize 		= add. latency : %f (sec)~
ocanal-son:srate 			= sampling rate: %.2f KHz~
ocanal-son:printtempname 	= temporary file name: %s~

ocanal-son:nosdii			= Sound Designer II files are only supported by the Mac version of SVP.~

ocanal-son:short            = %d bit integer~
ocanal-son:float            = %d bit floating point~
ocanal-son:packingformat    = packing format: %s~
ocanal-son:nonormalization  = no normalization of output file! ~
ocanal-son:normalizationlevel  = normalization level : -%.1lf dB ~

ocanal-son:mono             = mono~
ocanal-son:stereo           = stereo~
ocanal-son:quad             = quadraphonic~
ocanal-son:many             = multiphonic~
ocanal-son:nchans 			= number of channels: %s~

ocanal-son:info             = info : %d~
ocanal-son:meta             = meta : %d~

ocanal-spector:moduletype 	= \nocanal-spector output module~
ocanal-spector:allocerror 	= can't allocate memory for ocanal-spector data~
ocanal-spector:print 		=
\tformat         : binary \
\ttag            : number of samples (float) \
\tmask           : amplitude and phase ~


oca-data-f0:moduletype 	= \nocanal-f0 output module~
oca-data-f0:formatsdif   = output format : sdif (%d columns)~
oca-data-f0:formatascii  = output format : ascii~
oca-data-f0:formatbin    = output format : binary~

oca-data-glottis:moduletype 		= \nGlottic puls analysis output module~

ocanal-mem:onlymono   = Only single channel supported for callback based output module.~
ocanal-mem:unsupportedpacktype = unsupported data type/size for memory io!\n~
