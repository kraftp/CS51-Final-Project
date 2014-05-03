CS51-Final-Project
==================

CS51 Final Project Regular Expressions

USAGE INSTRUCTIONS:

	./Regex.native [-dp] [-dn] "REGEX" "INPUT"

REGEX is the input regular expression.  INPUT is the input string.  INPUT will 
be matched against REGEX.  Whether INPUT matches REGEX will be ouputed to
stdout. The [-dp] and [-dn] flags enable visualization of the parse tree and 
non-deterministic finite automaton (NFA) that the regular expression is parsed
and compiled into, respectively (see below).

Be sure to escape characters normally escaped in your shell with backslashes.
The characters '(', ')', '|', '*', '.', '?', '[', ']', and '~' need to be
escaped with the tilde, '~'.

GRAMMAR (BNF):

	<re> ::= <re1> | <re1> “|” <re>
	<re1> ::= <re2> | <re2> “*”
	<re2> ::= <re3> | <re3> “?”
	<re3> ::= <atom> | <atom> <re3>
	<atom> ::= “(“<re>”)” | <schar>
	<schar> ::= char | Wild | [char, char]

*Note that Wild, the wildcard, is denoted by a period in regexes

VISUALIZATION CODE:

Visualization is done using DOT.  Currently, both NFA's and parse trees
can be visualized.   The makedot function prints DOT code to stdout, 
from which it can be redirected into a file for compilation into an image 
of the inputted NFA. To download a DOT compiler, run:

	yum list available 'graphviz*'
	sudo yum install 'graphviz*'

Once installed, use the commands "neato  -Tpng -O *.dot or "dot  -Tpng -O *.dot"
to compile the DOT code into a PNG for easy viewing.
