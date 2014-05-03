(*** CS 51 FINAL PROJECT ***)
(*** PETER KRAFT LILLY SHEN KIMBERLEY YU KAT ZHOU***)

open Core.Std
open Emulator
open Nfa
open Parser
       
       
(*IMPLEMENT I/O HERE*)
       
let command = 
  Command.basic 
    ~summary: "Matches strings based on inputted regular expression!"



  Command.Spec.(empty
    +> flag "-dn" no_arg ~doc:" Generate DOT code for the NFA"
    +> flag "-dp" no_arg ~doc:" Generate DOT code for the parser"
    +> anon ("input regex" %: string)
    +> anon ("input string" %: string))

    (fun if_dot_nfa if_dot_pt regex input () -> 
       if if_dot_pt then Parse.makedot regex;
       let parse = Parse.parse regex in
       if if_dot_nfa then Auto.makedot parse;
       if not (if_dot_nfa || if_dot_pt) then
       let nfa = Auto.to_nfa parse in
         (match Emulate.eval input nfa with
	  | None -> Printf.printf 
		    "INVALID REGULAR EXPRESSION MATCHING IMPOSSIBLE\n\n"
	  | Some true -> Printf.printf "Matched! \n\n"
	  | Some false -> Printf.printf "No match :( \n\n"))
		   
let () = Command.run ~build_info: "Typically compiled with given Makefile" 
                     ~version: "First submitted version" command
		     

		     
