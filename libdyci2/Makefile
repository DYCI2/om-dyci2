dycitest: main.c libdyci2.so
		gcc main.c -I/System/Library/Frameworks/Python.framework/Versions/2.7/include/python2.7 -L./ -ldyci2 -o dycitest
		
libdyci2.so: dyci2.o
		gcc -lpython2.7 -o libdyci2.so -shared dyci2.o
#		gcc  dyci2.o -static -L/System/Library/Frameworks/Python.framework/Versions/2.7/lib/python2.7/config/ -lpython2.7 -lSystem -o libdyci2.so -shared

dyci2.o: dyci2.c
		gcc -I/System/Library/Frameworks/Python.framework/Versions/2.7/include/python2.7 -o dyci2.o -c dyci2.c

clean:
		rm -rf *.o

install:
		cp libdyci2.so ../om-dyci2/lib/mac/

mrproper:
		rm -rf libdyci2.so
		rm -rf ../om-dyci2/lib/mac/libdyci2.so
