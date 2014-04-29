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
    
    let rec eval_lst (str : char list) (auto : Auto.nfa) : bool = 
        match str with
        | [] -> if auto = Empty then true else false
        | hd :: tl -> 
              match auto with
              | Empty -> false
              | Single (chr, next) -> if (hd = chr) then eval_lst tl !next else false
              | Or (next1, next2) -> eval_lst str !next1 || eval_lst str !next2
              | Star (clos, next) -> eval_lst str !next || eval_lst str !clos
            
    let eval (str : string) (auto : Auto.nfa) : bool = 
        eval_lst (explode str) (auto)

end
 
