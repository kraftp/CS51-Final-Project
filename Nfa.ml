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
    
    val makedot : Parse.pt -> nfa
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
        | Empty       -> Empty
        | Single(c)   -> Single(c, ref Empty)
        | Cat(re1, re2) -> let ret = (to_nfa re1) in 
                           let second = (to_nfa re2) in List.iter
                 ~f:(fun x -> x := second) (lptr ret); ret 
        | Or(re1, re2)  -> Or(ref (to_nfa re1), ref (to_nfa re2))
        | Star(re)      -> let preret = (to_nfa re) in 
                           let ret = Star(ref preret, ref Empty) in List.iter
                 ~f:(fun x -> x := ret) (lptr preret); ret 
    
    type nfarec = {num: int; auto : nfa; }
    
    
    let rec nrchecker (inlist : nfarec list) (input: nfa ref) : int option =
    match inlist with
    |[]-> None
    |hd::tl -> if phys_equal hd.auto !input then Some hd.num else nrchecker tl input

    
    
    (*let rec dotter (graph : nfa) (x : int ref) : nfarec list = 
    let orig = !x in
    match graph with
    |Empty -> Printf.printf "%d [label=\"ACCEPT\"];\n" !x; x:=!x+1; []
    |Single (a, ptr) -> 
                        (Printf.printf "%d -> %d;\n" !x (!x+1);
                        Printf.printf "%d [label=\"%c\"];\n" !x a;
                        x:=!x+1; {num=orig; auto=(ref graph)}::(dotter !ptr x))
    |Star (ptr1, ptr2) ->
                        (Printf.printf "%d -> %d;\n" !x (!x+1);
                        Printf.printf "%d [label=\"*\"];\n" !x;
                        x:=!x+1; ignore(stardot !ptr1 x orig graph);
                        Printf.printf "%d -> %d;\n" orig (!x);
                        {num=orig; auto=(ref graph)}::(dotter !ptr2 x))   
    |Or (ptr1, ptr2) ->               
                        (Printf.printf "%d -> %d;\n" !x (!x+1);
                        Printf.printf "%d [label=\"|\"];\n" !x;
                        x:=!x+1; 
                        let ret = dotter !ptr1 x in
                        Printf.printf "%d -> %d;\n" orig (!x);
                        ignore(ordot !ptr2 x ret); {num=orig; auto=(ref graph)}::ret)
                        
    and stardot (graph : nfa) (x : int ref) (st : int) (nstar : nfa) : nfarec list=
    let orig = !x in
    match graph with
    |Empty -> Printf.printf "%d [label=\"ACCEPT\"];\n" !x; x:=!x+1; []
    |Single (a, ptr) -> if phys_equal !ptr nstar then
                        (Printf.printf "%d -> %d;\n" !x st;
                        Printf.printf "%d [label=\"%c\"];\n" !x a;
                        x:=!x+1; [])
                        else 
                        (Printf.printf "%d -> %d;\n" !x (!x+1);
                        Printf.printf "%d [label=\"%c\"];\n" !x a;
                        x:=!x+1; {num=orig; auto=(ref graph)}::stardot !ptr x st nstar)
    |Star (ptr1, ptr2) -> 
                         if phys_equal !ptr2 nstar then
                         (Printf.printf "%d -> %d;\n" !x st;
                         Printf.printf "%d [label=\"*\"];\n" !x;
                         x:=!x+1; stardot !ptr1 x orig graph )
                        else 
                        (Printf.printf "%d -> %d;\n" !x (!x+1);
                        Printf.printf "%d [label=\"*\"];\n" !x;
                        x:=!x+1; ignore(stardot !ptr1 x orig graph);
                        Printf.printf "%d -> %d;\n" orig (!x);
                        {num=orig; auto=(ref graph)}::stardot !ptr2 x st nstar)   
    |Or (ptr1, ptr2) ->               
                        (Printf.printf "%d -> %d;\n" !x (!x+1);
                        Printf.printf "%d [label=\"|\"];\n" !x;
                        x:=!x+1;
                        let ret = stardot !ptr1 x st nstar in
                        Printf.printf "%d -> %d;\n" orig (!x);
                        ignore(ordot !ptr2 x ret); {num=orig; auto=(ref graph)}::ret)*)
                        
                        
    let rec dotter (graph : nfa) (x : int ref) (nfaor : nfarec list) : nfarec list =
    let orig = !x in
    match graph with
    |Empty -> Printf.printf "%d [label=\"ACCEPT\"];\n" !x; x:=!x+1; []
    |Single (a, ptr) -> (match nrchecker nfaor ptr with
                        |Some num ->
                        (Printf.printf "%d -> %d;\n" !x num;
                        Printf.printf "%d [label=\"%c\"];\n" orig a;
                        x:=!x+1; [])
                        |None ->
                        (Printf.printf "%d -> %d;\n" orig (!x+1);
                        Printf.printf "%d [label=\"%c\"];\n" orig a;
                        x:=!x+1; {num=orig; auto=graph}::(dotter !ptr x nfaor)))
    |Star (ptr1, ptr2) -> 
                         (match nrchecker nfaor ptr2 with
                         |Some num ->
                         (Printf.printf "%d -> %d;\n" orig num;
                         Printf.printf "%d [label=\"*\"];\n" orig;
                         Printf.printf "%d -> %d;\n" orig (!x+1);
                         x:=!x+1; ignore(dotter !ptr1 x [{num=orig; auto=graph}]); [] )
                        |None ->
                        (Printf.printf "%d -> %d;\n" orig (!x+1);
                        Printf.printf "%d [label=\"*\"];\n" orig;
                        x:=!x+1; ignore(dotter !ptr1 x [{num=orig; auto=graph}]);
                        Printf.printf "%d -> %d;\n" orig (!x);
                        {num=orig; auto=graph}::(dotter !ptr2 x nfaor)))   
    |Or (ptr1, ptr2) ->               
                        (Printf.printf "%d -> %d;\n" orig (!x+1);
                        Printf.printf "%d [label=\"|\"];\n" orig;
                        x:=!x+1; 
                        let ret = dotter !ptr1 x nfaor in
                        (Printf.printf "%d -> %d;\n" orig (!x);
                        ignore(dotter !ptr2 x (nfaor@ret)); {num=orig; auto=graph}::ret))                      
                        
                        
    let rec makedot (parse : Parse.pt) : nfa =
    let thenfa = to_nfa parse in
    (Printf.printf "digraph G\n {\n size=\"20,20\";\n";
    ignore(dotter thenfa (ref 0) []); Printf.printf "}\n"; thenfa)
    
    
    (*TESTING TESTING TESTING TESTING HI KAT*)
    (*
    let testtree = 
        Cat(Single('C'), Cat(Single('S'), 
        Star(Paren(Or(Cat(Single('5'), Single('1')),
        Cat(Single('5'), Single('0'))))))) in
    to_nfa testtree*)
    

end



