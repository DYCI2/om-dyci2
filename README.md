# OM-DYCI2 library

OM-DYCI2 is a framework for interactive generation of musical sequences combining generative agents and computer-assisted composition tools. It enables guiding corpus-based generative processes through explicit specifications of temporal structures, or using another layer of generative models trained on a corpus of structures. 

OM-DYCI2 is the compositional declination of the Max [DYCI2 library dedicated to interaction](https://github.com/DYCI2/Dyci2Lib) developped in the [OM#](https://github.com/cac-t-u-s/om-sharp)/[OM6](http://repmus.ircam.fr/openmusic/) visual programming and computer-aided composition environments. ([Doc Python Library](http://repmus.ircam.fr/downloads/docs/DYCI2_library/).

© Victoire Siguret, Jean Bresson, Jérôme Nika — STMS lab IRCAM / CNRS / Sorbonne Université, 2018.
Contributions by Dionysios Papanikolaou and Mikhail Malt (2021).

If using the library, please quote: __Nika, Jérôme, and Jean Bresson. "Composing Structured Music Generation Processes with Creative Agents." 2nd Joint Conference on AI Music Creativity (AIMC 2021). 2021.[(Article)](https://aimc2021.iem.at/wp-content/uploads/2021/06/AIMC_2021_Nika_Bresson.pdf)__ if using the library. The appendix of this article refers to multimedia resources extracted from recent creations using OM-DYCI2 (Pascal Dusapin, Steve Lehman, Rémi Fox, Le Fresnoy - Studio National des Arts Contemporains, etc.). Moreover, it is a good introduction to the use of the library since it details all the playing modes implemented in the tutorials.

------

### Repository

This repository contains:
* __libdyci2__: a C wrapper to the DYCI2 Python library allowing to compile libdyci2 as a dynamic library.
* __om-dyci2__: a library for using DYCI2/libdyci2 in the OM/o7 computer-aided composition environment.

__Users are invited to use the pre-packed version of the **om-dyci2** source folder distributed in this project's [release pages](https://github.com/DYCI2/om-dyci2/releases)__, and developpers or expert users may compile the source code available on this github repository (see below).

------
### Installing and using om-dyci2:

**om-dyci2** can be used in either in [OM 6.13] or [OM#] environments. __To be able to use all the features, we recommend for the moment to use OM# (other features will be ported to OM in a future release).__

  * See [OM#](https://github.com/cac-t-u-s/om-sharp) manual page for OM#.
  * See [OM6 external libraries](http://repmus.ircam.fr/openmusic/libraries) manual page for OM 6.13/6.14.

__Tutorials__ are available in om-dyci2/patches/.

The **om-dyci2** source folder including the compiled wrapper (libdyci2/libdyci2.so) and the DYCI2 python modules (omdyci2/lib/python) must be installed in one of the OM library folders.
Alternatively, you can just specify this repository as one of the "Libraries folder" in the OM Preferences.

**Note:** om-dyci2 will instantiate a virtual Python interpreter and run DYCI2 in it, so **the last version of Python 3 and the dependencies of DYCI2 library must also be installed on your computer** 
:

1. Download and install the **last** version of Python 3 (https://www.python.org/downloads)

2. Use your terminal to install the dependencies:

    $ pip install -r python-requirements.txt

(If _pip_ is not installed: `sudo easy-install pip`.)

------
### Compile and install the C wrapper (libdyci2):

This library was only compiled and tested on macOS so far.

1. Download the last version of DYCI2lib [HERE](https://github.com/DYCI2/Dyci2Lib)

2. Move om-dyci2 and DYCI2lib in a same parent directory

3. Compile libdyci2: use your Terminal in [parent_directory]/om-dyci2/libdicy2/ and simply Make the library:

```
cd libdyci2
make
make install
```

The Makefile will assume that Python 3.9 is installed in /System/Library/Frameworks/Python.framework/

`make install` will copy the built dyci2lib.so to the adequate folder of the **om-dyci2** library (in *om-dyci2/lib/mac/*)

------

### Getting started

See the __'quick start' pdf guide__ and the user manual page and examples in this project's [wiki pages](https://github.com/DYCI2/om-dyci2/wiki).

The _om-dyci2/om-dyci2/patches_ directory includes basic example patches for OM# and OM6, and new advanced examples for OM# only (Tutorials 1 to 3) requiring the [OM-SuperVP library](https://forum.ircam.fr/projects/detail/om-supervp/).

OM-DYCI2 takes audio files with annotations (time markers and labels) as input. The library does not yet provide definitive tools for the segmentation and analysis of corpora, which must therefore be carried out by the user in the environment of his/her choice and have the format corresponding to the example annotation files that can be found in Tutorials 1 and 2. 
On the other hand, OM-DYCI2 proposes manual segmentation interfaces (Tutorial 0), and a first draft (work in progress) of segmentation patches and automatic pitch and energy analysis (Tutorial 3).

------
### Externals

The tutorials in the directories "2-advanced-tutorials-synchronyzing" and "3-tutorials-audio-analysis" require the OM-SuperVP library. 

1) Get it from https://forum.ircam.fr/projects/detail/om-supervp/. 

2) Move the superVP lib for instance in om-dyci2/dependencies/ and make sure that the path to this directory is in the search path.

3) This release includes SuperVP executables for macOS, Windows, Linux. These programs are protected and require an authorization through Ircam forum subscription. 
See: https://www.ircam.fr/product/abonnement-premium-individuel-annuel/ (see *** note below)

4) Then in OM#:
- windows/session window/external libraries then double click on om-supervp to load it
- preferences/libraries, in om-SuperVP :
	- SuperVP exec --> path to supervp exec (for instance dependencies/om-supervp/resources/bin/mac/SuperVP/SuperVP.app/Contents/MacOS/supervp-2.103.2)
	- Authorize --> path of the activation key if needed (for instance dependencies/om-supervp/resources/bin/mac/Ircam_Activation_key.txt)
 
Free-releases of the library (not including the binaries) are available at: https://github.com/openmusic-project/OM-SuperVP/releases.
You will still need the binaries (and then an Ircam forum subscription): geyt et them from IRCAM ForumNet (https://forum.ircam.fr/projects/detail/analysissynthesis-command-line-tools/) and either put them in OM-SuperVP/resources/bin/{mac,win,linux}/, OR set the path to SuperVP (for instance in .../AudioSculpt/Kernels/) in the OM Preferences/Externals path (visible when the library is loaded).

------
### Troubleshooting
Please write to `jerome.nika@ircam.fr` for any question, or to share with us your projects using OM-DYCI2 !

------
### License
GPL v3
