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
    Command.Spec.(
      empty
      +> flag "-d" no_arg ~doc:"Generate DOT code"
      +> anon ("input regex" %: string)
      +> anon ("input string" %: string)
    )
    (fun if_dot regex input () -> 
      let parse = Parse.parse regex in
      if if_dot then 
        Auto.makedot parse
      else
        let nfa = Auto.to_nfa parse in
        if (Emulate.eval input nfa) then Printf.printf ("Matched! \n")
        else Printf.printf "No match : sry bbz \n") 
    
    
    let () = Command.run command
    

 
