all : buffer-add.so

buffer-add.so : buffer-add.o
	raco ctool --ld buffer-add.so buffer-add.o

buffer-add.o : buffer-add.c
	raco ctool --cc buffer-add.c
