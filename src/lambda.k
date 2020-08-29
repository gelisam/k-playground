require "substitution.md"

module LAMBDA
  imports DOMAINS
  imports SUBSTITUTION
  syntax Val ::= KVar
               | "lambda" KVar "." Exp          [binder]
  syntax Exp ::= Val
               | Exp Exp                        [left, strict]
               | "(" Exp ")"                    [bracket]
  syntax Val ::= Int | Bool
  syntax Exp ::= Exp "*" Exp                    [strict, left]
               | Exp "/" Exp                    [strict]
               > Exp "+" Exp                    [strict, left]
               > Exp "<=" Exp                   [strict]
  syntax Exp ::= "if" Exp "then" Exp "else" Exp [strict(1)]
  syntax Exp ::= "let" KVar "=" Exp "in" Exp
  syntax Exp ::= "letrec" KVar KVar "=" Exp "in" Exp
               | "mu" KVar "." Exp              [binder]
  syntax KResult ::= Val
endmodule
