prefix = /m/lli_tl/lli_tlc/tools/app/local
bindir = $(prefix)/bin

srcdir = ../src
VPATH = ../src

CC = gcc
CPPFLAGS = -I$(srcdir)
CFLAGS = -Wall -W -O3
LDFLAGS = -s

.PHONY: all
all: triangle

triangle: triangle.c
	$(CC) $(CPPFLAGS) $(CFLAGS) -DNO_TIMER -DCPU86 -c $(srcdir)/triangle.c
	$(CC) $(LDFLAGS) -o $@ triangle.o

.PHONY: install
install: all
	test -d $(bindir) || mkdir $(bindir)
	install -c -m 755 triangle.exe $(bindir)

.PHONY: clean
clean: all
	rm -f triangle.exe
