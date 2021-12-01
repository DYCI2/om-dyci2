mixer-add:moduletype 	= \nMixer module (spectral addition)~
mixer-cross:moduletype 	= \nMixer module (cross synthesis)~
mixer-mul:moduletype 	= \nMixer module (spectral multiplication)~
mixer-mul:amplitudemode = using : amplitudes of second track only~
mixer-mul:phasemode     = using : phases of second track only~
mixer-mul:complete      = using : full spectrum of second channel~

mixer:fromfile 			= mixer parameter file: %s~
mixer:ampconst1 		= track one amp factor: %f~
mixer:ampconst2 		= track two amp factor: %f~
mixer:cofactor 			= multiplicative cofactor: %f~
mixer:phaseconst1 		= track one freq constant: %f~
mixer:phaseconst2 		= track two freq constant: %f~
mixer-cross:amp2exp             = exponent for amp product : %f~
mixer-cross:amp2exp2            = exponent range for amp prod : %f - %f~
mixer:ampfile1 			= amp factor file: %s~
mixer:cofactorfile 		= multiplicative cofactor file: %s~
mixer:phasefile1 		= phase factor file: %s~
mixer:endoffile 		= end of file `%s' reached when reading mixer coefficients\n ~
mixer:allocerror 		= can't allocate memory for a new mixer module~
mixer:openerror 		= error opening mixer parameter file `%s'\n ~
mixer:fileempty 		= mixer parameter file is empty~
mixer:nottimevarying 	= params must be time-varying in mixer coefficient file~
mixer:badffttype  = unsupported fft type detected in run_mixer!\n~
mixer:crossneedsampfrequ = 
  cross synthesis of type cross can only handle
  input coding of type amplitude/frequency. 
  Please report this bug together with your SuperVP 
  parameters to IRCAM.~

mixer:nomuladdwithampfrequ = 
   cross synthesis of type mul, pmul or add can not
   be applied if dilation is requested!~

mixer-cross:spsscale           = phase sync scale : %f~
