
all: music
music: Main.hs
	ghc --make Main.hs -o music
clean: 
	rm -rf *.o music
	rm -rf *.hi music