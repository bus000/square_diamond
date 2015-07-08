.PHONY: clean test

all:
	cd src/ ; make

test:
	cd src/ ; make clean
	cd src/ ; make
	cd test/ ; make

clean:
	cd src/ ; make clean
	cd test/ ; make clean
