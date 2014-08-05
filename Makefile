all:
	ghc main interpreter parser -O -o 3rd

clean:
	rm *.o *.hi
