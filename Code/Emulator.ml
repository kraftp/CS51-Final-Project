(*** CS 51 FINAL PROJECT ***)
(*** PETER KRAFT LILLY SHEN KIMBERLEY YU KAT ZHOU***)
 
open Core.Std
open Nfa
 
(*IMPLEMENT AN NFA EMULATOR HERE*)
 
 module type EMULATOR =
sig
    exception NotRecognized
    exception TODO
    
    val eval : string -> Auto.nfa option -> bool option
    
    
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
    
    let rec nsaux (auto : Auto.nfa) (n : int): (Auto.nfa list) =
        match auto with
        | Empty -> [Empty]
        | Single (_, _, x) -> if !x=n then [] else (x:=n; [auto])
        | Or (next1, next2) -> (nsaux !next1 n)@(nsaux !next2 n)
        | Star (next1, next2) -> (nsaux !next1 n)@(nsaux !next2 n)
        | Opt (next1, next2) -> (nsaux !next1 n)@(nsaux !next2 n)
        
    let next_states (auto : Auto.nfa) (n : int) : Auto.nfa list =
        match auto with
        | Single(_, ptr, _) -> nsaux !ptr n
        | _ -> nsaux auto n
        
    let checkmatch (c : char) (auto : Auto.nfa)  : bool =
        match auto with
        | Single (a, _, _) -> (match a with 
                           |Wild -> true
                           |Char(chr) -> chr=c)
        | Empty -> false
        | _ -> failwith "FAILURE IN checkmatch: ONLY SINGLES/EMPTIES HERE"
    
    let rec checkaccept (auto : Auto.nfa list) : bool =
        match auto with
        | [] -> false
        | hd::tl -> (hd=Empty)||(checkaccept tl)       
        
    let rec eval_lst (str : char list) (auto : Auto.nfa list) (count : int): bool =   
        match str with
        | [] -> checkaccept auto
        | hd :: tl -> 
            let checked = List.filter ~f:(checkmatch hd) auto in
            match checked with
            | [] -> false
            | _ -> let next = 
          (List.fold_left ~f:(fun (x : Graph.graph list) (a : Graph.graph) -> (next_states a count)@x) ~init:[] checked) in
                eval_lst tl next (count+1)
    

    let eval (str : string) (auto : Auto.nfa option) : bool option = 
        match auto with
        |None -> None
        |Some tn -> match tn with
                    |Single (_, _, _) -> Some (eval_lst (explode str) [tn] 2)
                    | _ -> Some (eval_lst (explode str) (next_states tn 1) 2)
end
 
