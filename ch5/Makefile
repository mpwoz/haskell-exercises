all: main json

main: Main.hs json
	ghc -o simple Main.hs

json: SimpleJSON.hs
	ghc -c SimpleJSON.hs

clean: 
	rm -f *.hi *.o
sweep: clean
	rm -f simple
