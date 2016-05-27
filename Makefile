# Makefile for LexNeo.x

Main: Main.hs SintNeo.hs LexNeo.hs
	ghc Main.hs -o SintNeo --make

SintNeo.hs: SintNeo.y
	happy SintNeo.y	

LexNeo.hs: LexNeo.x
	alex LexNeo.x

clean:
	rm *.o
	rm *.hi
