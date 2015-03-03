# Makefile for Linake ACO experiment
# AKM Khaled Ahsan Talukder
# George Mason University
# 08/12/2011

# Directories
INCDIR = /include
OBJDIR = /obj
SRCDIR = /src
BINDIR = /bin
UTILDIR = /clist

# Compiler
CC = gcc

# Compiler options
CFLAGS = -Wall -pedantic -g
IFLAGS = -I. -I.$(UTILDIR)/

# all object files
OBJ = randgen.o sort.o list.o probdef.o aco.o main.o

all: aco

aco: $(OBJ)
	$(CC) $(CFLAGS) $(IFLAGS) -lm $(OBJ) -o aco

randgen.o: .$(UTILDIR)/randgen.c .$(UTILDIR)/randgen.h
	$(CC) $(CFLAGS) $(IFLAGS) -c .$(UTILDIR)/randgen.c -o randgen.o

sort.o: .$(UTILDIR)/sort.c .$(UTILDIR)/sort.h
	$(CC) $(CFLAGS) $(IFLAGS) -c .$(UTILDIR)/sort.c -o sort.o

list.o: .$(UTILDIR)/list.c .$(UTILDIR)/list.h .$(UTILDIR)/sort.h
	$(CC) $(CFLAGS) $(IFLAGS) -c .$(UTILDIR)/list.c -o list.o

probdef.o: probdef.c probdef.h
	$(CC) $(CFLAGS) $(IFLAGS) -c probdef.c -o probdef.o

aco.o: aco.c aco.h
	$(CC) $(CFLAGS) $(IFLAGS) -c aco.c -lm -o aco.o

main.o: main.c
	$(CC) $(CFLAGS) $(IFLAGS) -c main.c -o main.o
clean:
	rm -f *.o
	rm -f aco
