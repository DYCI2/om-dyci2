/*
 *  dyci2.c
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

 #include "dyci2.h"

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*********************************
 * API ENTRY:
 *********************************
 * Init Python interpreter
 * Load DYCI2 module
 * Retrns a DICT containing all function objects references
 * pathToDyci2 must point to the DYCI2_Modules folder
 *
 *********************************/
void* Dyci2Init( char *pathToDyci2 , const char *fileName)
{	
	
	PyObject *pName, *pModule, *pDict;
	
	if( ! Py_IsInitialized() )
	{
		Py_Initialize();
	}

	PySys_SetPath( pathToDyci2 );

	// char *path = Py_GetPath();
  	// printf("PyPath: %s\n\n", path);	
	//PyObject *sys_path = PySys_GetObject("path");
	//PyList_Append( sys_path, PyString_FromString( pathToDyci2 ) );
		  	
    pName = PyString_FromString( fileName );
    pModule = PyImport_Import( pName ); 
	// pName not needed anymore
	Py_DECREF(pName);

	//pModule = PyImport_ImportModule("DYCI2_Modules.GeneratorBuilder"); // ?
		
	printf("Loaded module: %p\n", pModule);
	
	if (pModule == NULL) 
	{
		PyErr_Print();
		return NULL; 
	}
	else 
	{ 
		pDict = PyModule_GetDict(pModule);
		printf("Loaded dict: %p\n", pDict);
		// Py_DECREF(pModule); // => No! pModule is a borrowed reference.
    
		return pDict;
	}
}


/*********************************
 *
 * Quit Dyci2 / Python interpreter
 *
 *********************************/
int Dyci2Quit (void* pyPtr)
{
	Py_Finalize();
    return 0;
}


/*********************************
 *
 * Util: create a PyList
 *
 *********************************/
void* Dyci2MakeList( int size )
{
	return PyList_New( size );
}

/*********************************
 *
 * Util: free a PyList
 *
 *********************************/
int Dyci2FreeList( void* pylist )
{
	/* 
	for (int i = 0 ; i < PyList_Size( pylist ) ; i++) 
	{
		Py_DECREF( PyList_GetItem( pylist , i) );
	}
	*/

	Py_DECREF( pylist );
	return 0;
}

/*********************************
 *
 * Util: adds tring in a PyList
 *
 *********************************/
int Dyci2ListAddString( void* pyList, char* item, int pos )
{
	PyList_SetItem( pyList , pos, PyString_FromString( item ) );
	return pos;
}



/*********************************
 *
 * Util: call a Python function from C
 *
 *********************************/
void * Dyci2Call ( void* pyDict, const char* fName, int nb_arg, void *pArgs) // , void *KW) 
{
	
	printf("ENTER Dyci2Call for %s..\n", fName);
	// printf("dict: %p\n", pyDict);

    PyObject *returnValue = NULL;
 
    PyObject *pFunc, *pValue;

	//pDict = PyModule_GetDict(pModule); 
    
	pFunc = PyDict_GetItemString(pyDict, fName); // pFunc is a borrowed reference: no cleanup	
	printf("pFunc: %p\n", pFunc);
           		
    if ( PyCallable_Check( pFunc ) ) 
    {
        printf("function check (%s) => ok\n", fName);

        pValue = PyObject_CallObject(pFunc, pArgs );
        printf("return value: %p\n", pValue);
	
        if (pValue != NULL) 
    	{
	    	returnValue	=  pValue; // ici : savoir récupérer les types de pValue
    	}
    	else 
   		{
        	PyErr_Print();
    	}
    } 
    else 
    {
        PyErr_Print();
    }
		
	printf( "RETURN FROM DYCI2CALL: %s...\n", fName);
	
    return returnValue;
}




/*********************************
 * API ENTRY:
 *********************************
 *
 * Creates a generator
 *
 ***********************************/

void* Dyci2MakeGenerator( void *pyPtr, int size,  void *pySeq, void *pyLabels )
{
	
	PyObject *pyGen;

	PyObject *args = PyTuple_New(2);
	PyTuple_SetItem(args, 0, pySeq);
	PyTuple_SetItem(args, 1, pyLabels);

	pyGen = Dyci2Call(pyPtr, "Generator", 2, args);
	
	printf("==================== NEW GENERATOR: %p\n", pyGen);
	if (pyGen == NULL) 
	{
		PyErr_Print();
	} else {
		Py_INCREF( pyGen );
	}	

	Py_DECREF( args );
	return pyGen;
}

/*********************************
 * API ENTRY:
 *********************************
 *
 * Free a generator
 *
 ***********************************/
int Dyci2FreeGenerator( void *pyGen )
{
	Py_DECREF( pyGen );
	return 0;
}


/*********************************
 * API ENTRY:
 *********************************
 *
 * Query generator (returns err flag)
 *
 ***********************************/
int Dyci2GenQuery( void *pyPtr, void *Generator, int size, void *pyHandle )
{
	PyObject *args=PyTuple_New(1);
	PyTuple_SetItem(args, 0, pyHandle);

	printf("======================= Gen Query \n");	
	PyObject *pyQuery = Dyci2Call(pyPtr, "new_temporal_query_sequence_of_events", 1, args);
	printf("======================= ok!\n");

	Py_DECREF(args);
	
	if (pyQuery==NULL)
	{
		printf("======================= Error Query:\n");
		PyErr_Print();
		return -1;
	}
	else
	{
		printf("======================= RECEIVE_QUERY \n");
		PyObject_CallMethodObjArgs(Generator, PyString_FromString("receive_query") , pyQuery, NULL);
		printf("======================= OK!\n");

		Py_DECREF(pyQuery);
		
		return 1;
	}
}


// TODO
/*
void * Dyci2GenFreeQuery( void *pyPtr, void * Generator, int length )
{

	PyObject *pyLength, pyQuery;

	pyLength = PyLong_FromLong( length );
	
	PyObject *args=PyTuple_New(2);
	PyTuple_SetItem(args, 0, pyLength);
	PyTuple_SetItem(args, 1, PyString_FromString("Label"));

	pyQuery = Dyci2Call(pyPtr, "new_temporal_query_free_sequence_of_events", 1, args);

	if (pyQuery==NULL)
	{
		PyErr_Print();
		Py_DECREF(pyLength);
		return NULL;
	}
	else
	{
		PyObject_CallMethodObjArgs(Generator, PyString_FromString("receive_query") , pyQuery);
	}

	Py_DECREF(pyLength);
	return Generator;
}

*/



/*********************************
 *
 * Util: prints generator parameters
 *
 ***********************************/
int Dyci2ParametersMod(void *Generator)
{

	PyObject *item;
	char *res;

	PyObject *pyElem=PyObject_GetAttrString(Generator, "memory");
	if (pyElem == NULL)
	{
		PyErr_Print();
		return 1;
	}
	
	PyObject *pyOutput = PyObject_GetAttrString( pyElem , "control_parameters");
	if (pyOutput == NULL)
	{
		PyErr_Print();
		return 1;
	}

	Py_ssize_t len = PySequence_Size(pyOutput);
	for (int i=0; i< (int)len; i++)
	{
		item = PySequence_GetItem( pyOutput, i);
		res = PyString_AsString(item);
		printf("%s \n", res);
	}
	return 0;
}


/*********************************
 * API ENTRY:
 *********************************
 * Set a Generator parameter of type int
 ***********************************/
void *Dyci2SetParametersINT(void *Generator, char *parameter, int value)
{
	PyObject *pyElem=PyObject_GetAttrString(Generator, "memory");
	if (pyElem == NULL)
	{
		PyErr_Print();
		return NULL;
	}
	PyObject *pValue=PyLong_FromLong((long) value);
	int output = PyObject_SetAttrString( pyElem , parameter, pValue);
	if (output==-1)
	{
		PyErr_Print();
		return NULL;
	}

	return Generator;
}




/*********************************
 * API ENTRY:
 *********************************
 * 
 * Returns the size of the current output
 *
 **********************************/
int Dyci2GenOutputSize( void * Generator )
{
	// printf("======================= ENTER GEN OUTPUT SIZE:\n");
		
	PyObject *pyOutput = PyObject_GetAttrString(Generator, "current_generation_output");
	int rep;

	if (pyOutput == NULL)
	{
		PyErr_Print();
		return -1;
	}
	else
	{
		Py_ssize_t len = PySequence_Size(pyOutput);
		// rep = PyLong_AsLong( len );
		return (int) len;
	}
}


/*********************************
 * API ENTRY:
 *********************************
 *
 * Return a strings corresponding to Nth element in current output
 *
 *********************************/
char * Dyci2GenNthOutput( void * Generator, int n )
{
	// printf("======================= ENTER GEN NTH OUTPUT: %d\n", n);

	PyObject *pyOutput, *elem;
		
	pyOutput = PyObject_GetAttrString( Generator , "current_generation_output");

	if (pyOutput == NULL)
	{
		PyErr_Print();
		return "ERROR";
	}

	elem = PySequence_GetItem( pyOutput, n);
	if (elem == NULL)
	{
		PyErr_Print();
		return "ERROR";
	}
	else
	{
		char * res = PyString_AsString(elem);
		// printf("OUTPUT %s\n", res);
		return res;
	}
}



















