(*** CS 51 FINAL PROJECT ***)
(*** PETER KRAFT LILLY SHEN KIMBERLEY YU KAT ZHOU***)
 
open Core.Std
open Parser
 
(*BUILD AN NFA HERE*)
 
  module type AUTOMATON =
sig
    exception NotRecognized
    exception TODO
    
    type nfa
    
    val lptr : nfa -> nfa list (* list of all pointers to empty *)
    val tonfa : Parse.pt -> nfa
    
    
end


module Auto : AUTOMATON =
struct

    exception NotRecognized
    exception TODO
        
    type nfa = 
	Empty
      | Single of char * nfa ref (* char and forward pointer to next node *)
      | Or of nfa ref * nfa ref (* forward pointers to 2 or options *)
      | Star of nfa ref * nfa ref (* forward pointers into and out of closure *)
    
    
        
    let tonfa (parse : Parse.pt) : nfa = "bob the builder"    

end
