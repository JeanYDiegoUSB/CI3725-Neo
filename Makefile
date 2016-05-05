# Makefile for LexNeo.x

LexNeo: LexNeo.hs
	ghc LexNeo.hs -o LexNeo --make
	
LexNeo.hs: LexNeo.x
	alex LexNeo.x
	
clean:
	rm *.o
	rm *.hi
