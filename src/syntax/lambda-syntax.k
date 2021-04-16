require "substitution.md"

module LAMBDA-SYNTAX
  imports DOMAINS
  imports SUBSTITUTION

  syntax Type ::= "int"                                                        [klabel(#int),symbol]
                | Type "->" Type                                               [klabel(#arrow),symbol,right]

  syntax Val ::= Int
  syntax Exp ::= Val
               | "(" Exp ")"                                                   [bracket]
               | Exp Exp                                                       [klabel(#apply),symbol,left]
               > Exp "+" Exp                                                   [klabel(#plus),symbol,left]
               | KVar
               > "let" KVar "=" Exp "in" Exp                                   [klabel(#let),symbol,binder]
               | "lambda" KVar "->" Exp                                        [klabel(#lambda),symbol,binder]
               | "\\" KVars "->" Exp

  syntax KVars ::= KVar
                 | KVar KVars
  rule \ X -> E => lambda X -> E                                               [macro]
  rule \ X XS -> E => lambda X -> \ XS -> E                                    [macro]
endmodule
