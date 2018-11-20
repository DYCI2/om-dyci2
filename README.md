# om-dyci2
DYCI2 for the OpenMusic ([OM6](http://repmus.ircam.fr/openmusic/)/[om7](https://openmusic-project.github.io/)) visual programming and computer-aided composition environments.

© Victoire Siguret, Jean Bresson, Jérôme Nika — STMS lab IRCAM / CNRS / Sorbonne Université, 2018.

This repository contains:
* __libdyci2__: a C wrapper to the DYCI2 Python library allowing to compile libdyci2 as a dynamic library.
* __om-dyci2__: a library for using DYCI2/libdyci2 in the OM/o7 computer-aided composition environment.

------
### About DYCI2:

http://repmus.ircam.fr/downloads/docs/DYCI2_library/

------

### Installing and using om-dyci2:

**om-dyci2** can be used in either in [OM 6.13] or [o7] environments.
  * See [om7 external libraries](https://openmusic-project.github.io/pages/libraries) manual page for om7.
  * See [OM6 external libraries](http://repmus.ircam.fr/openmusic/libraries) manual page for OM 6.13/6.14.

The **om-dyci2** source folder including the compiled wrapper (libdyci2/libdyci2.so) must be installed in one of the OM library folders.
Alternatively, you can just specify this repository as one of the "Libraries folder" in the OM Preferences.
This can be achieved using the pre-packed version distributed in this project's [release pages](https://github.com/DYCI2/om-dyci2/releases)), or by compiling the source code (see below).

__Tutorials__ are available in om-dyci2/patches/.

**Note:** om-dyci2 will instanciate a virtual Python interpreter and run DYCI2 in it, so **the last version of Python 2.7 and the dependencies of DYCI2 library must also be installed on your computer** (get the latest version [HERE](https://github.com/DYCI2/Dyci2Lib) !).

------
### Compile and install the C wrapper (libdyci2):

This library was only compiled and tested on macOS so far.

Download the last version of DYCI2lib [HERE](https://github.com/DYCI2/Dyci2Lib), move om-dyci2 and DYCI2lib in a same parent directory, and compile libdyci2. Use your Terminal in [parent_directory]/om-dyci2/libdicy2/ and simply Make the library:
```
cd libdyci2
make
make install
```

The Makefile will assume that Python 2.7 is installed in /System/Library/Frameworks/Python.framework/

`make install` will copy the built dyci2lib.so to the adequate folder of the **om-dyci2** library (in *om-dyci2/lib/mac/*)





------
### Getting started

See the user manual page and examples in this projesct's [wiki pages](https://github.com/DYCI2/om-dyci2/wiki)
