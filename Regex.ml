(*** CS 51 FINAL PROJECT ***)
(*** PETER KRAFT LILLY SHEN KIMBERLEY YU KAT ZHOU***)
 
open Core.Std
open Emulator
open Nfa
open Parser
 
 
(*IMPLEMENT I/O HERE*)
 
 let cs = Parse.parse "(a*a|b|as|da)|q*sdfs(f|s)" in
 (* Auto.makedot cs *)
     let cs2 = Auto.to_nfa cs in 
  assert((Emulate.eval "b" cs2)&&(Emulate.eval "qqqqsdfsf" cs2)&&(not (Emulate.eval "sa" cs2)))

 
