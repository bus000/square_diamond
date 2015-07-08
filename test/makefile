CC        =gcc
CFLAGS    =-Wall -ansi -Werror
SOURCES   =$(wildcard *.c)$
OBJECTS   =$(SOURCES:.c=.o)$
EXOBJECTS =$(shell find '../src/' -name '*.o' | grep -v main.o)$
TARGETS   =$(SOURCES:.c=)$
LFLAGS    =-L../src/

.PHONY: clean $(TARGETS)

all: $(TARGETS)

$(TARGETS): $(OBJECTS)
	echo $<
	echo $@
	$(CC) $(CFLAGS) $< $(EXOBJECTS) -o $@ $(LFLAGS)
	./$@ < $@_input.txt > $@_output.txt
	diff $@_correct_output.txt $@_output.txt

%.o: %.c
	$(CC) -o $@ $(CFLAGS) -c $<

clean:
	rm -f $(OBJECTS) $(TARGETS)
	rm -f *output.txt
