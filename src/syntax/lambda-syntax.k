require "substitution.md"

module LAMBDA-SYNTAX
  imports DOMAINS
  imports SUBSTITUTION

  syntax KResult
  syntax Focus ::= Exp | KResult
  syntax Hyps ::= Focus "==>" KVar "&" Hyps                                    [strict(1), right]
                | "----" "[" Args "]"
  syntax Args ::= String | Args "," Arg
  syntax Arg ::= KVar | KResult
  rule R:KResult ==> X & Hyps => Hyps[R / X]


  syntax Type ::= "int" | "bool" | Type "->" Type

  syntax Val ::= KVar
               | "lambda" KVar ":" Type "." Exp                                [binder]
  syntax Exp ::= Val
               | Exp Exp                                                       [left]
               | "(" Exp ")"                                                   [bracket]
  syntax Val ::= Int | Bool
  syntax Exp ::= Exp "*" Exp                                                   [left]
               > Exp "+" Exp                                                   [left]
               > Exp "<=" Exp
  syntax Exp ::= "if" Exp "then" Exp "else" Exp
  syntax Exp ::= "let" KVar ":" Type "=" Exp "in" Exp
  syntax Exp ::= "letrec" KVar KVar ":" Type "=" Exp "in" Exp
               | "mu" KVar ":" Type "." Exp                                    [binder]

  rule let X : T = E in E':Exp => (lambda X : T . E') E                        [macro]
  rule letrec F:KVar X:KVar : T1 -> T2 = E in E'
    => let F : T1 -> T2 = mu F : T1 -> T2 . lambda X : T1 . E in E'            [macro]
endmodule
