CS51-Final-Project
==================

CS51 Final Project Regular Expressions

VISUALIZATION CODE:

Visualization is done using DOT.  Currently, only NFA's can be visualized.  
The makedot function prints DOT code to stdout, from which it can be 
redirected into a file for compilation into an image of the inputted NFA. 
To download DOT, run:

	yum list available 'graphviz*'
	sudo yum install 'graphviz*'

One installed, use the commands "neato  -Tpng -O *.dot or "dot  -Tpng -O *.dot"
to compile the DOT code into a PNG for easy viewing.