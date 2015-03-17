CC      =gcc
CFLAGS  =-Wall -ansi
SOURCES =$(wildcard *.c)$
OBJECTS =$(SOURCES:.c=.o)$
TARGET  =square_diamond

.PHONY: clean

all: $(TARGET)

$(TARGET): $(OBJECTS)
	$(CC) $(CFLAGS) $(OBJECTS) -o $(TARGET)

%.o: %.c
	$(CC) -o $@  $(CFLAGS) -c $<

clean:
	rm -f $(OBJECTS) $(TARGET)
