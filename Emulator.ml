(*** CS 51 FINAL PROJECT ***)
(*** PETER KRAFT LILLY SHEN KIMBERLEY YU KAT ZHOU***)
 
open Core.Std
open Nfa
 
(*IMPLEMENT AN NFA EMULATOR HERE*)
 
 module type EMULATOR =
sig
    exception NotRecognized
    exception TODO
    
    val eval : string -> Auto.nfa -> bool
    
    
end


module Emulate : EMULATOR =
struct

    exception NotRecognized
    exception TODO
        
    (* taken from caml.inria.fr/mantis/view.php?id=5367 
       to convert strings to list of characters *)
    let explode s =
        let rec exp i l = 
            if i< 0 then l else exp (i-1) (s.[i] :: l) in
        exp (String.length s - 1)[];;
        
    let rec implode slist str = 
        match slist with
        | [] -> str
        | hd :: tl -> implode tl (str ^ (String.make 1 hd))
    
    (* checks if there is matching in a star *)    
    let rec check_star (orig : Auto.nfa ref) (clos : Auto.nfa ref) (str : char list) : bool * char list = 
        match str with
        | [] -> (false, [])
        | hd :: tl -> 
            match !clos with
            | Empty -> failwith "There shouldn't be an empty ref in the closure"
            | Single (chr, next) -> if (hd = chr) then check_star orig next tl 
                                    else if (orig = clos) then (true, str)
                                    else (false, [])
            | Or (next1, next2) -> check_star orig next1 str || check_star orig next2 str
            | Star (nclos, nnext) -> if (check_star orig nclos str) then check_star orig nnext str
                                     else (false, [])
    
    let rec eval_lst (str : char list) (auto : Auto.nfa) : bool = 
        match str with
        | [] -> if (auto = Empty) then true else false
        | hd :: tl -> 
              match auto with
              | Empty -> false
              | Single (chr, next) -> if (hd = chr) then eval (implode tl) !next else false
              | Or (next1, next2) -> eval str next1 || eval str next2
              | Star (clos, next) -> match (check_star clos clos str) with
                                     | (false, _) -> false
                                     | (true, nstr) -> eval_lst nstr !next
            
    let eval (str : string) (auto : Auto.nfa) : bool = 
        eval_list (explode str) (auto)
        

end
 
