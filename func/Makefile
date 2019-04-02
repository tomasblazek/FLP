# Název: simplify-bkg
# Autor: Tomáš Blažek (xblaze31) 

FILENAME = simplify-bkg
ARCHIVENAME = flp-fun-xblaze31

all: build

build:
	ghc --make $(FILENAME).hs

run:
	./$(FILENAME) -2

clean:
	rm *.hi *.o $(FILENAME)

zip:
	zip $(ARCHIVENAME).zip *.hs Makefile README.md

cleanZip: 
	rm $(ARCHIVENAME).zip
