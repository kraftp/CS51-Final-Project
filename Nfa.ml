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
    
    val to_nfa : Parse.pt -> nfa
    
    
end


module Auto : AUTOMATON =
struct

    exception NotRecognized
    exception TODO
        
    (* Cat and Paren don't need their own nfa types because they're just combinations of nfas or groupings of nfas below *)
    type nfa = 
	    Empty
      | Single of char * nfa ref (* char and forward pointer to next node *)
      | Or of nfa ref * nfa ref (* forward pointers to 2 or options *)
      | Star of nfa ref * nfa ref (* forward pointers into and out of closure *)
      

    (* lptr constructs a list of all pointers to Empty in the input nfa *)
    let rec lptr (input : nfa) : nfa ref list =
        match input with
        | Empty -> failwith "should have been caught earlier"
        | Single(_, ptr) -> if !ptr = Empty then [ptr] else lptr !ptr 
        | Or(ptr1, ptr2) -> (lptr !ptr1)@(lptr !ptr2)
        | Star (_, ptr2) -> if !ptr2 = Empty then [ptr2] else lptr !ptr2 

    
    let rec to_nfa (parse : Parse.pt) : nfa = 
        match parse with
        | Empty       -> failwith "Trees aren't empty.  They have leaves."
        | Single(c)   -> Single(c, ref Empty)
        | Cat(re1, re2) -> let ret = (to_nfa re1) in List.iter
                 ~f:(fun x -> x := (to_nfa re2)) (lptr ret); ret 
        | Or(re1, re2)  -> Or(ref (to_nfa re1), ref (to_nfa re2))
        | Star(re)      -> let ret = (to_nfa re) in List.iter
                 ~f:(fun x -> x := (Star(ref ret, ref Empty))) (lptr ret); ret 
    
    
    
    (*TESTING TESTING TESTING TESTING HI KAT*)
    (*
    let testtree = 
        Cat(Single('C'), Cat(Single('S'), 
        Star(Paren(Or(Cat(Single('5'), Single('1')),
        Cat(Single('5'), Single('0'))))))) in
    to_nfa testtree*)
    

end



