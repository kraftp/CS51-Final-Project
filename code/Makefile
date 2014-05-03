SOURCES = \
Parser.ml \
Emulator.ml \
Nfa.ml 

all: $(SOURCES)
	corebuild -quiet Regex.native

clean:
	rm -rf _build *.native *.out
