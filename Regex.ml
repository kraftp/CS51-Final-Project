(*** CS 51 FINAL PROJECT ***)
(*** PETER KRAFT LILLY SHEN KIMBERLEY YU KAT ZHOU***)
 
open Core.Std
open Emulator
open Nfa
open Parser
 
 
(*IMPLEMENT I/O HERE*)
 
 let bob = Parse.parse "a*t|((a|b|c|d)*q(a)*d)*" in
 Auto.makedot bob
