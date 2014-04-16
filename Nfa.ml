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
    
    val tonfa : Parse.pt -> nfa
    
    
end


module Auto : AUTOMATON =
struct

    exception NotRecognized
    exception TODO
        
    type nfa = string (**TODO**)
    
        
    let tonfa (parse : Parse.pt) : nfa = "bob the builder"    

end
