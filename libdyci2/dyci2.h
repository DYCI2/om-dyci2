/*
 *  dyci2.h
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

#include "Python.h"


// initializes Python / loads module / returns the pDict required to make Python calls to DYCI2
void* Dyci2Init( char * chemin, const char * fichier );

// clears memory
int Dyci2Quit( void *pObj );

// utils
void* Dyci2MakeList( int size );
int Dyci2ListAddString( void* pyList, char* item, int pos );
int Dyci2FreeList( void* pylist );

// creates a generator
void* Dyci2MakeGenerator( void *pyPtr, int size,  void* pSeq, void *pLabels );

// query generator (returns err flag)
int Dyci2GenQuery( void *pyPtr, void *Generator, int size, void *pyHandle );
//void * Dyci2GenFreeQuery( void *pyPtr, void * Generator, int length );

// prints generator parameters
int Dyci2ParametersMod( void *Generator );

// set generator param
void *Dyci2SetParametersINT( void *Generator, char *parameter, int value );

// returns the size of the current output
int Dyci2GenOutputSize( void * Generator );
// return a strings corresponding to Nth element in current output
char * Dyci2GenNthOutput( void * Generator , int n );

