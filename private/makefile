all : buffer-add.dylib

buffer-add.dylib : buffer-add.o
	raco ctool --ld buffer-add.dylib buffer-add.o

buffer-add.o : buffer-add.c
	raco ctool --cc buffer-add.c
