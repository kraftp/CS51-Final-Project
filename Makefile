all: modules ps4
	corebuild parser.native
	corebuild emulator.native
	corebuild nfa.native

clean:
	rm -rf _build *.native
