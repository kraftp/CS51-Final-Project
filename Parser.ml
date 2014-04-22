(*** CS 51 FINAL PROJECT ***)
(*** PETER KRAFT LILLY SHEN KIMBERLEY YU KAT ZHOU***)
 
open Core.Std
 
(*IMPLEMENT A PARSER HERE*)

module Tree =
struct
    type tree = (*parse tree*)
         Empty 
        |Single of char
        |Paren  of tree (*Parenthesis*)
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
    
    let tokenizer (str : string) : token list
              
    let parse (str : string) : pt = raise TODO    

end



