(*** CS 51 FINAL PROJECT ***)
(*** PETER KRAFT LILLY SHEN KIMBERLEY YU KAT ZHOU***)
 
open Core.Std
 
(*IMPLEMENT A PARSER HERE*)
 
module type PARSER =
sig
    exception NotRecognized
    exception TODO
    
    type pt
    
    val parse : string -> pt
    
    
end


module Parse : PARSER =
struct

    exception NotRecognized
    exception TODO

    type pt = (*parse tree*)
         Empty 
        |Single of char
        |Paren  of pt (*Parenthesis*)
        |Cat    of pt * pt (*Concatenation*)
        |Or     of pt * pt 
        |Star   of pt (*Kleene Star/Closure*)
        
        
    let parse (str : string) : pt = raise TODO    

end



