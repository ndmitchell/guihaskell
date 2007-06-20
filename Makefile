all:
	ghc --make -Wall -i../proplang -o guihaskell Main.hs
clean:
	rm *.o
	rm *.hi
