(*** CS 51 FINAL PROJECT ***)
(*** PETER KRAFT LILLY SHEN KIMBERLEY YU KAT ZHOU***)

open Core.Std
open Parser
       
(*BUILD AN NFA HERE*)
module Graph = 
  struct 
    type graph = 
        Empty
      | Single of Tree.schar * graph ref * int ref (* char and forward pointer to next node *)
      | Or of graph ref * graph ref  (* forward pointers to 2 or options *)
      | Star of graph ref * graph ref (* forward pointers into and out of closure *)
      | Opt of graph ref * graph ref (* forward pointers into and out of option  *)
  end
    
module type AUTOMATON =
  sig
    exception NotRecognized
    exception TODO
		
    type nfa = Graph.graph
		 
    val to_nfa : Parse.pt option -> nfa option
					
    val makedot : Parse.pt option -> unit
  end


module Auto : AUTOMATON =
  struct
    exception NotRecognized
    exception TODO
		
    (* Cat and Paren don't need their own nfa types because they're just combinations of nfas or groupings of nfas below *)
    type nfa = Graph.graph
		 
    (* lptr constructs a list of all pointers to Empty in the input nfa *)
    let rec lptr (input: nfa) : nfa ref list =
      match input with
      | Empty -> failwith "ERROR IN lptr:  should have been caught earlier"
      | Single(_, ptr, _)  -> if !ptr = Empty then [ptr] else lptr !ptr 
      | Or(ptr1, ptr2)     -> (lptr !ptr1)@(lptr !ptr2)
      | Star (_, ptr2)     -> if !ptr2 = Empty then [ptr2] else lptr !ptr2 
      | Opt  (ptr1 , ptr2) -> if !ptr2 = Empty then ptr2::(lptr !ptr1)
                              else (lptr !ptr1)@(lptr !ptr2)
						  
    let rec to_nfa_aux (parse : Parse.pt) : nfa = 
      let (empty : nfa) = Empty in
      match parse with
      | Single(c)     -> Single(c, ref empty, ref 0)
      | Cat(re1, re2) -> let ret = (to_nfa_aux re1) in 
                         let second = (to_nfa_aux re2) in 
			 List.iter ~f:(fun x -> x := second) (lptr ret); ret 
      | Or(re1, re2)  -> Or(ref (to_nfa_aux re1), ref (to_nfa_aux re2))
      | Star(re)      -> let preret = (to_nfa_aux re) in 
                         let (ret : nfa) = Star(ref preret, ref empty) in 
			 List.iter ~f:(fun x -> x := ret) (lptr preret); ret 
      | Opt(re)       -> Opt(ref (to_nfa_aux re), ref empty) 
			    
    let to_nfa (parse : Parse.pt option) : nfa option =
      match parse with
      | None -> None
      | Some tn -> Some (to_nfa_aux tn)
			
    (*Visualization.  Outputs DOT code which can be compiled to pictures later*)
			
    type nfarec = {num: int; auto : nfa}
		    
    let rec nrchecker (inlist : nfarec list) (input: nfa ref) : int option =
      match inlist with
      | [] -> None
      | hd::tl -> if phys_equal hd.auto !input 
		  then Some hd.num else nrchecker tl input
									       
    let rec dotter (graph: nfa) (x: int ref) (nfaor: nfarec list): nfarec list =
      let orig = !x in
      match graph with
      | Empty -> Printf.printf "%d [label=\"ACCEPT\"];\n" !x; x:=!x+1; []
      | Single (a, ptr, _) -> 
         (match nrchecker nfaor ptr with
          | Some num ->
             (Printf.printf "%d -> %d;\n" !x num;
             (match a with 
              | Wild -> Printf.printf "%d [label=\"WILD\"];\n" orig
              | Char(c)-> Printf.printf "%d [label=\"%c\"];\n" orig c
              | Charclass(a, b) ->
		 Printf.printf "%d [label=\"[%c, %c]\"];\n" orig a b); 
                 x:=!x+1; [])
          | None ->
             (Printf.printf "%d -> %d;\n" orig (!x+1);
              (match a with 
               | Wild -> Printf.printf "%d [label=\"WILD\"];\n" orig
               | Char(c) -> Printf.printf "%d [label=\"%c\"];\n" orig c
               | Charclass(a, b) -> 
                  Printf.printf "%d [label=\"[%c, %c]\"];\n" orig a b); 
	          x:=!x+1; {num=orig; auto=graph}::(dotter !ptr x nfaor)))
      | Star (ptr1, ptr2) -> 
         (match nrchecker nfaor ptr2 with
          | Some num ->
             (Printf.printf "%d -> %d;\n" orig num;
              Printf.printf "%d [label=\"STAR\"];\n" orig;
              Printf.printf "%d -> %d;\n" orig (!x+1);
              x:=!x+1; ignore(dotter !ptr1 x [{num=orig; auto=graph}]); [] )
          | None ->
             (Printf.printf "%d -> %d;\n" orig (!x+1);
              Printf.printf "%d [label=\"STAR\"];\n" orig;
              x:=!x+1; ignore(dotter !ptr1 x [{num=orig; auto=graph}]);
              Printf.printf "%d -> %d;\n" orig (!x);
              {num=orig; auto=graph}::(dotter !ptr2 x nfaor)))   
      | Opt (ptr1, ptr2) -> 
         (match nrchecker nfaor ptr2 with
          | Some num ->
             (Printf.printf "%d -> %d;\n" orig num;
              Printf.printf "%d -> %d;\n" orig (!x+1);
              Printf.printf "%d [label=\"OPT\"];\n" orig;
              x:=!x+1; 
              dotter !ptr1 x nfaor)
          | None ->                       
            (Printf.printf "%d -> %d;\n" orig (!x+1);
             Printf.printf "%d [label=\"OPT\"];\n" orig;
             x:=!x+1; 
             let ret = dotter !ptr2 x nfaor in
             (Printf.printf "%d -> %d;\n" orig (!x);
              ignore(dotter !ptr1 x (nfaor@ret)); {num=orig; auto=graph}::ret)))
      | Or (ptr1, ptr2) ->               
         (Printf.printf "%d -> %d;\n" orig (!x+1);
          Printf.printf "%d [label=\"OR\"];\n" orig;
          x:=!x+1; 
          let ret = dotter !ptr1 x nfaor in
          (Printf.printf "%d -> %d;\n" orig (!x);
          ignore(dotter !ptr2 x (nfaor@ret)); {num=orig; auto=graph}::ret))

    let makedot (parse : Parse.pt option) : unit =
      match parse with
      | None -> Printf.printf "INVALID NFA DOT PRINTING IMPOSSIBLE\n\n"
      | Some tn -> let thenfa = to_nfa_aux tn in
		   (Printf.printf "digraph G\n {\n size=\"20,20\";\n";
		    ignore(dotter thenfa (ref 0) []); Printf.printf "}\n")
  end



