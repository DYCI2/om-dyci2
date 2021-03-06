# om-dyci2
[DYCI2](https://github.com/DYCI2/Dyci2Lib) for the OpenMusic ([OM6](http://repmus.ircam.fr/openmusic/)/[OM#](https://cac-t-u-s.github.io/om-sharp/)) visual programming and computer-aided composition environments.

© Victoire Siguret, Jean Bresson, Jérôme Nika — STMS lab IRCAM / CNRS / Sorbonne Université, 2018.

This repository contains:
* __libdyci2__: a C wrapper to the DYCI2 Python library allowing to compile libdyci2 as a dynamic library.
* __om-dyci2__: a library for using DYCI2/libdyci2 in the OM/OM# computer-aided composition environment.

------
### About DYCI2:

http://repmus.ircam.fr/downloads/docs/DYCI2_library/

------

### Installing and using om-dyci2:

**om-dyci2** can be used in either in [OM 6.x] or [OM#] environments.
  * See [OM# external libraries](https://cac-t-u-s.github.io/om-sharp/pages/libraries) manual page for OM#.
  * See [OM6 external libraries](https://openmusic-project.github.io/libraries) manual page for OM.

__Tutorials__ are available in om-dyci2/patches/.

You can use the pre-packed version of the **om-dyci2** source folder distributed in this project's [release pages](https://github.com/DYCI2/om-dyci2/releases), or compile the source code available on this github repository (see below).

The **om-dyci2** source folder including the compiled wrapper (libdyci2/libdyci2.so) and the DYCI2 python modules (omdyci2/lib/python) must be installed in one of the OM library folders.
Alternatively, you can just specify this repository as one of the "Libraries folder" in the OM Preferences.

**Note:** om-dyci2 will instanciate a virtual Python interpreter and run DYCI2 in it, so **the last version of Python 3 and the dependencies of DYCI2 library must also be installed on your computer** 
:

1. Download and install Python 3.9 (https://www.python.org/downloads)

2. Use your terminal to install the dependencies:

```
$ pip3 install -r python-requirements.txt
```

(If _pip_ is not installed: `sudo easy_install install pip`.)

------
### Compile and install the C wrapper (libdyci2):

This library was only compiled and tested on macOS so far.

1. Clone or download the last version of DYCI2lib: (https://github.com/DYCI2/Dyci2Lib)

2. Move om-dyci2 and DYCI2lib in a same parent directory

3. Compile libdyci2: set your Terminal in `om-dyci2/libdicy2/` and simply Make the library:

```
make
make install
```

`make install` will copy the built dyci2lib.so and required Dyci2 python sources in *om-dyci2/lib/mac/*

Running these sources will be possible only if Python3.9 is installed



------
### Getting started

See the user manual page and examples in this projesct's [wiki pages](https://github.com/DYCI2/om-dyci2/wiki).
