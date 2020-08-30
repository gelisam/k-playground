require "lambda-syntax.k"

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
