CC      =gcc
CFLAGS  =-Wall -ansi -Werror
SOURCES =$(wildcard *.c)$
OBJECTS =$(SOURCES:.c=.o)$
TARGET  =square_diamond
LFLAGS  =-I/usr/local/include/libpng14 -L/usr/local/lib -lpng -lz

.PHONY: clean

all: $(TARGET)

$(TARGET): $(OBJECTS)
	$(CC) $(CFLAGS) $(OBJECTS) -o $(TARGET) $(LFLAGS)

%.o: %.c
	$(CC) -o $@  $(CFLAGS) -c $<

clean:
	rm -f $(OBJECTS) $(TARGET)
