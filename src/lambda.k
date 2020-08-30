require "substitution.md"

module LAMBDA-SYNTAX
  imports DOMAINS
  imports SUBSTITUTION

  syntax Val ::= KVar
               | "lambda" KVar "." Exp          [binder]
  syntax Exp ::= Val
               | Exp Exp                        [left, strict]
               | "(" Exp ")"                    [bracket]
  syntax Val ::= Int | Bool
  syntax Exp ::= Exp "*" Exp                    [strict, left]
               > Exp "+" Exp                    [strict, left]
               > Exp "<=" Exp                   [strict]
  syntax Exp ::= "if" Exp "then" Exp "else" Exp [strict(1)]
  syntax Exp ::= "let" KVar "=" Exp "in" Exp
  syntax Exp ::= "letrec" KVar KVar "=" Exp "in" Exp
               | "mu" KVar "." Exp              [binder]
endmodule

module LAMBDA
  imports LAMBDA-SYNTAX

  syntax KResult ::= Val

  rule (lambda X:KVar . E:Exp) V:Val => E[V / X]
  rule I1 * I2 => I1 *Int I2
  rule I1 + I2 => I1 +Int I2
  rule I1 <= I2 => I1 <=Int I2
  rule if true  then E else _ => E
  rule if false then _ else E => E
  rule let X = E in E':Exp => (lambda X . E') E [macro]
  rule letrec F:KVar X:KVar = E in E'
    => let F = mu F . lambda X . E in E'        [macro]
  rule mu X . E => E[(mu X . E) / X]
endmodule
