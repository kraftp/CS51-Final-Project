(*** CS 51 FINAL PROJECT ***)
(*** PETER KRAFT LILLY SHEN KIMBERLEY YU KAT ZHOU***)
 
open Core.Std
 
(*IMPLEMENT A PARSER HERE*)

module Tree =
struct

    type schar = Char of char | Wild | Charclass of char * char
    
    
    type tree = (*parse tree*)
          Single of schar
        | Cat    of tree * tree (*Concatenation*)
        | Or     of tree * tree 
        | Star   of tree (*Kleene Star/Closure*)
        | Opt    of tree (*Option/'?' Operator*)
        
end

module type PARSER =
sig
    exception NotRecognized
    exception TODO
    
    type pt = Tree.tree
    
    val parse : string -> pt option
    
    val makedot : string -> unit 
    
end

module Parse : PARSER =
struct

    exception NotRecognized
    exception TODO

    type pt = Tree.tree
    
    type token = Oper of char | Char of Tree.schar | Error
    
    (* taken from caml.inria.fr/mantis/view.php?id=5367 
       to convert strings to list of characters *)
    let explode (s : string) : char list =
        let rec exp (i : int) (l : char list) = 
            if (i < 0) then l else exp (i-1) (s.[i] :: l) in
        exp (String.length s - 1)[];;
    
    (*Converts character list to tokens, which are the smallest
      combinations of characters that have meaning.  Implements
      escape sequences*)
    let rec tokenizer (clist : char list) : token list =
        match clist with
        | [] -> []    
        | hd::tl -> 
            match hd with
            | '(' -> Oper('(')::(tokenizer tl)
            | ')' -> Oper(')')::(tokenizer tl)
            | '|' -> Oper('|')::(tokenizer tl)
            | '*' -> Oper('*')::(tokenizer tl)
            | '?' -> Oper('?')::(tokenizer tl)
            | '[' -> Oper('[')::(tokenizer tl)
            | ']' -> Oper(']')::(tokenizer tl)
            | '.' -> Char(Wild)::(tokenizer tl)
            | '~' -> (match tl with
                     | '('::tl2 -> Char(Char('('))::(tokenizer tl2)
                     | ')'::tl2 -> Char(Char(')'))::(tokenizer tl2)
                     | '|'::tl2 -> Char(Char('|'))::(tokenizer tl2)
                     | '*'::tl2 -> Char(Char('*'))::(tokenizer tl2)
                     | '.'::tl2 -> Char(Char('.'))::(tokenizer tl2)
                     | '?'::tl2 -> Char(Char('?'))::(tokenizer tl2)
                     | '['::tl2 -> Char(Char('['))::(tokenizer tl2)
                     | ']'::tl2 -> Char(Char(']'))::(tokenizer tl2)
                     | '~'::tl2 -> Char(Char('~'))::(tokenizer tl2)
                     | _ -> Printf.printf "Warning: Escape Character ~ Is Unused (Ignored).\n"; 
                            tokenizer tl)
            |  _  -> Char(Char(hd))::(tokenizer tl)
            
    (*Post-processor for the tokenizer that converts tokens related
      to character classes into a special character class token. *)         
    let rec charclasser (tlist : token list) : token list =
        match tlist with
        |[] -> []
        |hd1::tl -> 
            match hd1 with
            | Oper('[') -> 
               (match tl with
                |hd2::hd3::hd4::hd5::tl2 ->
                    (match (hd2, hd3, hd4, hd5) with
                    | (Char(Char(a)), Char(Char('-')), Char(Char(b)), Oper(']')) ->
                        if a < b then Char(Charclass(a, b))::(charclasser tl2)
                        else [Error]
                    | _ -> [Error])
                | _ -> [Error])
            | Oper(']') -> [Error]
            | _ -> hd1::(charclasser tl)
    
    
    (*The following seven functions check regular expressions to make sure
      they are valid, meaning that they have balanced and matched parenthesis
      only have gramatically meaningful combinations of operators, begin and 
      end with valid operators, and actually contains some characters to match
      against. *)
    let rec checkchar (tlist: token list) : bool =
        match tlist with
	      | [] -> Printf.printf "Error: Regex Contains No Characters\n"; false
	      | hd::tl -> match hd with
		    | Char(_) -> true
		    | Oper(_) -> checkchar tl
		    | Error -> Printf.printf "Error:  Improper Character Classes\n"; false
		    
    let rec checkerror (tlist : token list) : bool =
        match tlist with
        | [] -> true
        | hd::tl -> match hd with
                | Error -> Printf.printf "Error:  Improper Character Classes\n"; false
                | _ -> checkerror tl
        
		    
    let checkparen (tlist : token list) : bool =
      let rec cpaux (plist : int list) = function
        | [] -> (match plist with
            | [] -> true
            | _  -> Printf.printf "Error:  Mismatched Parentheses\n"; false)
        | hd::tl -> match hd with
                | Oper('(') -> cpaux (0::plist) tl
                | Oper(')') -> (match plist with
                            | [] -> Printf.printf "Error:  Unbalanced Parentheses\n"; false
                            | _::tlp -> cpaux tlp tl)
                | _  -> cpaux plist tl in cpaux [] tlist

    let rec checkdbl (tlist : token list) : bool = 
      match tlist with
      | [] -> true
      |[hd] -> (match hd with
               |Oper('|') -> Printf.printf "Error:  Regex Ends with Invalid Operator\n"; false
               | _ -> true)
      | hd1::hd2::tl -> 
	    match hd1 with
      | Oper('(') ->
           (match hd2 with
	          | Oper(')')| Oper('|')| Oper('*')| Oper('?')  ->
	            Printf.printf "Error:  Regex Contains Invalid Operator after '('\n"; false
	          | _ -> checkdbl (hd2::tl))
	    | Oper('*')| Oper('?')  ->
	       (match hd2 with
	        | Oper('*')| Oper('?')  -> 
	          Printf.printf "Error:  Regex Contains Invalid Operator after '?' or '*'\n"; false
	        | _ -> checkdbl (hd2::tl))
	    | Oper('|') ->
	        (match hd2 with
	           | Oper(')')| Oper('|')| Oper('*')| Oper('?') ->
	          Printf.printf "Error:  Regex Contains Invalid Operator after '|'\n"; false
	         | _ -> checkdbl (hd2::tl))
	    | _ -> checkdbl (hd2::tl)

    let checkfirst (tlist : token list) : bool =
      match tlist with 
      | [] -> Printf.printf "Error:  Empty Regex\n"; false
      | hd::_ -> match hd with
	    | Oper('*')|Oper(')')|Oper('|')|Oper('?')  ->
         	Printf.printf "Error:  Regex Begins with Invalid Operator\n"; false
	    | _ -> true
	    
	let checker (tlist : token list) : bool =
	checkfirst tlist && checkchar tlist && checkdbl tlist && checkparen tlist && checkerror tlist


    (*These five mutually recursive functions actually implement a recursive-
      descent parser.  There is one function per type of operator in the 
      grammar. *)
    let rec orfun (tlist : token list) : token list * pt =
        let (ntlist, nptree) = catfun tlist in
        match ntlist with
        | [] -> ([], nptree)
        | hd::tl -> 
            match hd with
            | Oper('|') -> let (listret, treeret) = orfun tl in (listret, Or(nptree, treeret))
	          | Oper(')') -> (tl, nptree)
            | _ -> (ntlist, nptree)   
            
    and catfun (tlist : token list)  : token list * pt = 
        let (ntlist, nptree) = starfun tlist in      
        match ntlist with
        | [] -> ([], nptree)
        | hd::_ -> 
            match hd with
            | Oper('|')| Oper(')')   -> (ntlist, nptree) 
            | _ -> let (listret, treeret) = catfun ntlist in (listret, Cat(nptree, treeret))              
                              
    and starfun (tlist : token list)  : token list * pt = 
        let (ntlist, nptree) = qfun tlist in
        match ntlist with
        | [] -> ([], nptree) 
        | hd::tl -> 
            match hd with
            | Oper('*') -> (tl, Star(nptree))
            | _ -> (ntlist, nptree)         

    and qfun (tlist : token list)  : token list * pt = 
        let (ntlist, nptree) = pfun tlist in
        match ntlist with
        | [] -> ([], nptree) 
        | hd::tl -> 
            match hd with
            | Oper('?') -> (tl, Opt(nptree))
            | _ -> (ntlist, nptree) 
         
    and pfun (tlist : token list) : token list * pt =
         match tlist with
         | [] -> failwith "Invalid Regular Expression (pfun 1)"
         | hd::tl -> 
             match hd with
             | Char(a) -> (tl, Single(a))
             | Oper('(') -> orfun tl
             | _ -> failwith "Invalid Regular Expression (pfun 2)"
            
            
     (*This is the exposed function that is actually called from the 
       command line and itself calls every other function*)
               
    let parse (str : string) : pt option = 
        let input = charclasser (tokenizer (explode str)) in
	      if checker input
	      then let (_, answer) = orfun input in Some answer else None
	      
    (*Visualization.  Outputs DOT code which can be compiled to pictures later*)	      
	      
    let rec dotter (tree : pt) (x : int ref) : unit =
    let orig = !x in
    match tree with
    |Single (a) ->      ((match a with
                        | Wild -> Printf.printf "%d [label=\"WILD\"];\n" orig
                        | Charclass(a, b) -> Printf.printf "%d [label=\"[%c, %c]\"];\n" orig a b
                        | Char(c) -> Printf.printf "%d [label=\"%c\"];\n" orig c);
                        x:=!x+1)
                        
    |Cat (t1, t2) ->   
                        (Printf.printf "%d -> %d;\n" orig (!x+1);
                        Printf.printf "%d [label=\"CAT\"];\n" orig;
                        x:=!x+1; dotter t1 x;
                        Printf.printf "%d -> %d;\n" orig !x;
                        dotter t2 x)
    |Or (t1, t2) ->   
                        (Printf.printf "%d -> %d;\n" orig (!x+1);
                        Printf.printf "%d [label=\"OR\"];\n" orig;
                        x:=!x+1; dotter t1 x;
                        Printf.printf "%d -> %d;\n" orig !x;
                        dotter t2 x)      
    |Star (t1)   ->  
                        (Printf.printf "%d -> %d;\n" orig (!x+1);
                        Printf.printf "%d [label=\"STAR\"];\n" orig;
                        x:=!x+1; dotter t1 x)
    |Opt  (t1)   ->  
                        (Printf.printf "%d -> %d;\n" orig (!x+1);
                        Printf.printf "%d [label=\"OPT\"];\n" orig;
                        x:=!x+1; dotter t1 x)                        
                        
    let makedot (str : string) : unit = 
        let input = charclasser (tokenizer (explode str)) in
	      if checker input
	      then let (_, itree) = orfun input in 
	        (Printf.printf "digraph G\n {\n size=\"20,20\";\n";
               dotter itree (ref 0); Printf.printf "}\n") 
	      else Printf.printf "ERROR INVALID REGULAR EXPRESSION\n\n"      
end



