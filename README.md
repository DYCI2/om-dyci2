# om-dyci2
An adapted version of DYCI2 for the OpenMusic ([OM](http://repmus.ircam.fr/openmusic/)/[o7](https://openmusic-project.github.io/)) visual programming and computer-aided composition environments.

© Victoire Siguret, Jean Bresson, Jérôme Nika — STMS lab IRCAM / CNRS / Sorbonne Université, 2018.

This repository contains:
* __libdyci2__: a C wrapper to the DYCI2 Python library allowing to compile libdyci2 as a dynamic library.
* __om-dyci2__: a library for using DYCI2/libdyci2 in the OM/o7 computer-aided composition environment.

------
### Compile and install (libdyci2):

This library was only compiled and tested on macOS so far.
Use your Terminal and simply Make the library:
```
cd libdyci2
make
make install
```

The Makefile will assume that Python 2.7 is installed in /System/Library/Frameworks/Python.framework/

`make install` will copy the built dyci2lib.so to the adequate folder of the **om-dyci2** library (in *om-dyci2/lib/mac/*)

------
### Installing om-dyci2:

No need to compile. 

**om-dyci2** can be used in either in [OM 6.13] or [o7] environments.
  * See [o7 external libraries](https://openmusic-project.github.io/pages/libraries) manual page for o7.
  * See [OM6 external libraries](http://repmus.ircam.fr/openmusic/libraries) manual page for OM 6.13.

The **om-dyci2** source folder including libdyci2.so (or the pre-packed version distributed in this project's [release pages](https://github.com/DYCI2/om-dyci2/releases)) must be installed in one of the OM/o7 library folders.

Alternatively, you can just specify this repository as one of the "Libraries folder" in the OM/o7 Preferences.

**Note:** om-dyci2 will instanciate a virtual Python interpreter and run DYCI2 in it, so **the DYCI2 library must also be installed on your computer** (get the latest version [HERE]() !).

After loading om-dyci2 for the first time, go to your OM/o7 Preferences/Libaries and set the correct path to the _DYCI2_Modules_ folder.

------
### Getting started

See the user manual page and examples in this projesct's [wiki pages](https://github.com/DYCI2/om-dyci2/wiki)
