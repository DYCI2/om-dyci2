dycitest: main.c libdyci2.so
	gcc main.c -I/Library/Frameworks/Python.framework/Versions/3.9/include/python3.9 -Wno-unused-result -Wsign-compare -Wunreachable-code -fno-common -dynamic -DNDEBUG -g -fwrapv -O3 -Wall -arch x86_64 -g -L./ -ldyci2 -o dycitest

libdyci2.so: dyci2.o
	gcc -L/Library/Frameworks/Python.framework/Versions/3.9/lib/python3.9/config-3.9-darwin -ldl -framework CoreFoundation -lpython3.9 -o libdyci2.so -shared dyci2.o
#	gcc  dyci2.o -static -L/System/Library/Frameworks/Python.framework/Versions/2.7/lib/python2.7/config/ -lpython2.7 -lSystem -o libdyci2.so -shared

dyci2.o: dyci2.c
	gcc -I/Library/Frameworks/Python.framework/Versions/3.9/include/python3.9 -Wno-unused-result -Wsign-compare -Wunreachable-code -fno-common -dynamic -DNDEBUG -g -fwrapv -O3 -Wall -arch x86_64 -g -o dyci2.o -c dyci2.c

clean:
	rm -rf *.o

install:
	mkdir -p ../om-dyci2/lib/mac/
	mkdir -p ../om-dyci2/lib/python/
	cp libdyci2.so ../om-dyci2/lib/mac/
	cp -R ../../Dyci2Lib/Python_library/DYCI2_Modules/ ../om-dyci2/lib/python/DYCI2_Modules
	cp -R ../../Dyci2Lib/Python_library/load.py ../om-dyci2/lib/python/

mrproper:
	rm -rf libdyci2.so
	rm -rf ../om-dyci2/lib/mac/libdyci2.so
	rm -rf ../om-dyci2/lib/python/
	rm -rf ../**/*.~