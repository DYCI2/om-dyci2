ampli:connectformaterror = Amplifier requires temporal input data!! Check your parameter flags !!~
ampli:badtremtype 			= unknown tremolo type, so I'll use a sine-wave~
ampli:allocerror 			= can't allocate memory for amplification module~
ampli:paramfile 			= parameter file: %s~
ampli:endoffile 			= warning: end of file `%s' reached at time %f\
\tgain parameters will be constant from now on\n ~

ampli-envelop:openerror 	= error opening envelope parameter file: %s\n ~
ampli-envelop:moduletype 	= \nEnvelope follower amplification module~
ampli-envelop:power 		= power: %f~
ampli-envelop:empty 		= envelope parameter file is empty~

ampli-gain:openerror 		= error opening gain parameter file~
ampli-gain:moduletype 		= \nGain follower amplification module~
ampli-gain:gain 			= gain: %f~
ampli-gain:allocerror 		= can't allocate memory for gain follower~
ampli-gain:empty 			= gain follower parameter file is empty~
ampli-gain:nonegdir             = reverse time processing not (yet) supported in the gain module!~

ampli-iir:moduletype            = IIR-Filter Amplification module ~
ampli-iir:numstep               = Numerator step : %d ~
ampli-iir:num                   = Numerator coef : %f ~
ampli-iir:denstep               = Denominator step : %d ~
ampli-iir:den                   = Denominator coef : %f ~
ampli-iir:paramsopenerr 	= error opening gain parameter file <%s> ~
ampli-iir:paramsstrerr          = error in paramsstr <%s> ~

ampli-tremolo:openerror 	= error opening tremolo parameter file~
ampli-tremolo:allocerror 	= can't allocate memory for tremolo~
ampli-tremolo:badtype 		= unknown tremolo type~
ampli-tremolo:moduletype 	= \nTremolo amplification module~
ampli-tremolo:type 			= tremolo type: %s~
ampli-tremolo:sin 			= sine~
ampli-tremolo:square 		= square~
ampli-tremolo:saw 			= sawtooth~
ampli-tremolo:triangle 		= triangle~
ampli-tremolo:frequency 	= frequency: %f~
ampli-tremolo:depth 		= depth: %f~
ampli-tremolo:empty 		= tremolo parameter file is empty~
ampli-tremolo:varfreq 		= variable frequency of tremolo not supported by amplitude module~
ampli-tremolo:freqneg 		= non-positive frequency encountered in tremolo parameter file~
