CC        =gcc
CFLAGS    =-Wall -ansi -Werror
SOURCES   =$(wildcard *.c)$
OBJECTS   =$(SOURCES:.c=.o)$
EXOBJECTS =$(shell find '../src/' -name '*.o' | grep -v main.o)$
TARGETS   =$(SOURCES:.c=)$
LFLAGS    =-L../src/ -I/usr/local/include/libpng14 -L/usr/local/lib -lpng -lz

.PHONY: clean $(TARGETS)

all: $(TARGETS)

$(TARGETS): $(OBJECTS)
	$(CC) $(CFLAGS) $@.o $(EXOBJECTS) -o $@ $(LFLAGS)
	./$@ > $@_output.txt
	diff $@_correct.txt $@_output.txt | colordiff

%.o: %.c
	$(CC) -o $@ $(CFLAGS) -c $<

clean:
	rm -f $(OBJECTS) $(TARGETS)
	rm -f *output.txt
