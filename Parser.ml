(*** CS 51 FINAL PROJECT ***)
(*** PETER KRAFT LILLY SHEN KIMBERLEY YU KAT ZHOU***)
 
open Core.Std
 
(*IMPLEMENT A PARSER HERE*)

module Tree =
struct
    type tree = (*parse tree*)
         Empty 
        |Single of char
        |Cat    of tree * tree (*Concatenation*)
        |Or     of tree * tree 
        |Star   of tree (*Kleene Star/Closure*)
        
end

module type PARSER =
sig
    exception NotRecognized
    exception TODO
    
    type pt = Tree.tree
    
    val parse : string -> pt   
    
end

module Parse : PARSER =
struct

    exception NotRecognized
    exception TODO

    type pt = Tree.tree
    
    type token = Oper of char | Char of char
    
    (* taken from caml.inria.fr/mantis/view.php?id=5367 
       to convert strings to list of characters *)
    let explode s =
        let rec exp i l = 
            if i< 0 then l else exp (i-1) (s.[i] :: l) in
        exp (String.length s - 1)[];;
    
    let rec tokenizer (clist : char list) : token list =
        match clist with
        | [] -> []    
        | hd::tl -> 
            match hd with
            | '(' -> Oper('(')::(tokenizer tl)
            | ')' -> Oper(')')::(tokenizer tl)
            | '|' -> Oper('|')::(tokenizer tl)
            | '*' -> Oper('*')::(tokenizer tl)
            (*IN THE FUTURE ESCAPE SEQUENCES*)
            | _   -> Char(hd)::(tokenizer tl)
         
    let checker (tlist: token list) : bool =
        match tlist with
	| [] -> false
	| hd::tl -> match hd with
		    | Char(_) -> true
		    | Oper(_) -> checker tl

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
        | hd::tl -> 
            match hd with
            |Oper('|')|Oper(')')   -> (ntlist, nptree) 
            | _ -> let (listret, treeret) = catfun ntlist in (listret, Cat(nptree, treeret))               
                              
    and starfun (tlist : token list)  : token list * pt = 
        let (ntlist, nptree) = pfun tlist in
        match ntlist with
        | [] -> ([], nptree) 
        | hd::tl -> 
            match hd with
            | Oper('*') -> (tl, Star(nptree))
            | _ -> (ntlist, nptree)         
         
    and pfun (tlist : token list) : token list * pt =
         match tlist with
         | [] -> failwith "Invalid Regular Expression (pfun 1)"
         | hd::tl -> 
             match hd with
             | Char(a) -> (tl, Single(a))
             | Oper('(') -> orfun tl
             | Oper(')') -> pfun tl
             | _ -> failwith "Invalid Regular Expression (pfun 2)"
            
    let parse (str : string) : pt = 
        let input = tokenizer (explode str) in
	if checker then let (_, answer) = orfun input in answer else None
end



