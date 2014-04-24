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
         
    let rec orfun (tlist : token list) : token list * pt =
        let (ntlist, nptree) = starfun tlist in
        match ntlist with
        | [] -> ([], nptree)
                  
    and starfun (tlist : token list)  : token list * pt = 
        let (ntlist, nptree) = catfun tlist in
        match ntlist with
        | [] -> ([], nptree) (*COME BACK TO THIS LATER*)
        | hd::tl -> 
            match hd with
            | Oper(')')|Oper('|')  -> (ntlist, nptree) (*some of this is wrong.  not sure what*)
            | Oper('*') -> (ntlist, Star(nptree))
            | _ -> (ntlist, nptree)         
              
    and catfun (tlist : token list)  : token list * pt = (*kat having fun*)
        let (ntlist, nptree) = pfun tlist in      
        match ntlist with
        | [] -> ([], nptree) (*COME BACK TO THIS LATER*)
        | hd::tl -> 
            match hd with
            | Oper(')')|Oper('|')|Oper('*')  -> (ntlist, nptree) (*some of this is wrong.  not sure what*)
            | _ -> let (listret, treeret) = pfun ntlist in (listret, Cat(nptree, treeret))
         
    and pfun (tlist : token list) : token list * pt =
         match tlist with
         | [] -> failwith "hope not" (*PROBABLY WRONG*)
         | hd::tl -> 
             match hd with
             | Char(a) -> (tl, Single(a))
             | Oper('(') -> orfun tl
            
    let parse (str : string) : pt = raise TODO    
end



