/*
 *  main.c
 *
 *  libdyci2: a simple C API for DYCI2
 *  Copyright (C) 2018 V. Siguret, J. Bresson, J. Nika, STMS Lab IRCAM/CNRS/Sorbonne Université
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; see the file COPYING. If not, write to the
 *  Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

/*
 * A SIMPLE TEST PROGRAM FOR libdyci2
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "dyci2.h"

// SET THE PATH TO /DYCI2_library/DYCI2_Modules/

#define PATH_TO_DYCI2 "/Users/bresson/SRC/Dyci2Lib/Python_library/DYCI2_Modules/"
//#define PATH_TO_DYCI2 "/Users/siguret/DYCI2_library/DYCI2_Modules/"


int main(int argc, char **argv) 
{
	int err;
	printf("================== Start DYCI2 TEST n");

	void *pyPtr = Dyci2Init(PATH_TO_DYCI2, "load");
	
	if ( pyPtr == NULL ) 
	{
		printf("================== ERROR INITAILIZING DYCI2: %p\n", pyPtr);
		Dyci2Quit( pyPtr );	
		return -1;
	}
	else 
	{	
		printf("================== DYCI2 init ok: %p\n", pyPtr);

		int size = 11;

		char *cSequence[] = {"A1", "B1", "B2", "C1", "A2", "B3", "C2", "D1", "A3", "B4", "C3"};
		char *cLabels[] = {"A", "B", "B", "C", "A", "B", "C", "D", "A", "B", "C"};

		void* pSeq = Dyci2MakeList( size );
		void* pLab = Dyci2MakeList( size );

		for (int i=0 ; i < size ; i++) 
		{ 
			Dyci2ListAddString(pSeq, cSequence[i], i );
			Dyci2ListAddString(pLab, cLabels[i], i );
		}
		
		
		printf("================== Create generator...\n");
		void* Gen = Dyci2MakeGenerator( pyPtr, size, pSeq, pLab );
		printf("================== GENERATOR: %p \n", Gen);	

		for ( int n = 1; n < 10 ; n++ )
		{
			
			printf("======================================================================== QUERY #%d\n", n);

			int querySize = 15;
			char *cHandle[] = {"C", "A", "B", "B", "C", "D", "C", "C", "D", "A", "A", "A", "A", "C", "B"} ;

			void* queryHandle = Dyci2MakeList( querySize );
			for (int i=0 ; i < querySize ; i++) 
			{ 
				Dyci2ListAddString(queryHandle, cHandle[i], i );
			}
		
			printf("================== Call Dyci2GenQuery...\n");
			err = Dyci2GenQuery(pyPtr, Gen, querySize, queryHandle );

			Dyci2FreeList ( queryHandle );

			int l = Dyci2GenOutputSize( Gen );
		
			printf( "================== OUTPUT [size=%d]:\n", l);
			for ( int i = 0; i < l ; i++ ) 
			{
				printf( "%s ", Dyci2GenNthOutput( Gen , i ));
			}
			printf("\n\n\n");
			sleep(3);
		}
			
		//err = Dyci2ParametersMod(Gen);
		//Dyci2SetParametersINT(Gen, "max_continuity", 5);
		
		printf("\n================== DONE\n");
		
		Dyci2Quit( pyPtr);	
		
		return 0;
	}
}












