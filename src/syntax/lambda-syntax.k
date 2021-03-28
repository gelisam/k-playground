require "substitution.md"

module LAMBDA-SYNTAX
  imports DOMAINS
  imports SUBSTITUTION

  syntax Type ::= "int"                                                        [klabel(#int),symbol]

  syntax Val ::= Int
  syntax Exp ::= Val
               | "(" Exp ")"                                                   [bracket]
               | Exp "+" Exp                                                   [klabel(#plus),symbol,left]
               | KVar
               > "let" KVar ":" Type "=" Exp "in" Exp                          [klabel(#let),symbol,binder]
endmodule
