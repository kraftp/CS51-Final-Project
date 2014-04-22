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
        
    type nfa = 
	    Empty
      | Single of char * nfa ref (* char and forward pointer to next node *)
      | Or of nfa ref * nfa ref (* forward pointers to 2 or options *)
      | Star of nfa ref * nfa ref (* forward pointers into and out of closure *)
      
    let rec lptr (input : nfa) : nfa ref list =
        match input with
          Empty -> failwith "should have been caught earlier"
        | Single(_, ptr) -> if !ptr = Empty then [ptr] else lptr !ptr 
        | Or(ptr1, ptr2) -> (lptr !ptr1)@(lptr !ptr2)
        | Star (_, ptr2) -> if !ptr2 = Empty then [ptr2] else lptr !ptr2 
     
        
    let tonfa (parse : Parse.pt) : nfa = Empty    

end
