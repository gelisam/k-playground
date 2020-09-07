require "substitution.md"

module LAMBDA-SYNTAX
  imports DOMAINS
  imports SUBSTITUTION

  syntax KResult
  syntax Focus ::= Exp | KResult
  syntax Hyps ::= Focus "==>" KVar "&" Hyps       [strict(1), right]
                | "----" "[" Args "]"
  syntax Args ::= String | Args "," Arg
  syntax Arg ::= KVar | KResult
  rule R:KResult ==> X & Hyps => Hyps[R / X]


  syntax Val ::= KVar
               | "lambda" KVar "." Exp          [binder]
  syntax Exp ::= Val
               | Exp Exp                        [left]
               | "(" Exp ")"                    [bracket]
  syntax Val ::= Int | Bool
  syntax Exp ::= Exp "*" Exp                    [left]
               > Exp "+" Exp                    [left]
               > Exp "<=" Exp
  syntax Exp ::= "if" Exp "then" Exp "else" Exp
  syntax Exp ::= "let" KVar "=" Exp "in" Exp
  syntax Exp ::= "letrec" KVar KVar "=" Exp "in" Exp
               | "mu" KVar "." Exp              [binder]

  rule let X = E in E':Exp => (lambda X . E') E [macro]
  rule letrec F:KVar X:KVar = E in E'
    => let F = mu F . lambda X . E in E'        [macro]
  rule mu X . E => E[(mu X . E) / X]
endmodule
