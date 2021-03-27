require "substitution.md"

module LAMBDA-SYNTAX
  imports DOMAINS

  syntax Type ::= "int"

  syntax Val ::= Int
  syntax Exp ::= Val
               | "(" Exp ")"                                                   [bracket]
  syntax Exp ::= Exp "+" Exp                                                   [left]
endmodule
