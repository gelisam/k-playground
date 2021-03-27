require "substitution.md"

module LAMBDA-SYNTAX
  imports DOMAINS
  imports SUBSTITUTION

  syntax Type ::= "int"

  syntax Val ::= Int
  syntax Exp ::= Val
               | "(" Exp ")"                                                   [bracket]
               | Exp "+" Exp                                                   [left]
               | KVar
               > "let" KVar ":" Type "=" Exp "in" Exp                          [right,binder]
endmodule
