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
        
        
    let eval (str : string) (auto : Auto.nfa) : bool = raise TODO    

end
 
