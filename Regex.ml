(*** CS 51 FINAL PROJECT ***)
(*** PETER KRAFT LILLY SHEN KIMBERLEY YU KAT ZHOU***)
 
open Core.Std
open Emulator
open Nfa
open Parser
 
 
(*IMPLEMENT I/O HERE*)
 
 let bob = Parse.parse "ababab*(a|b)qq*d" in
 Auto.makedot bob
