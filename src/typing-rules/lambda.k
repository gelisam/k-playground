require "../syntax/lambda-syntax.k"

module LAMBDA
  imports LAMBDA-SYNTAX

  syntax Exp ::= Type
  syntax KResult ::= Type


  rule _:Int
    => int

  rule _:Bool
    => bool

  rule E1 + E2
    => E1 ==> !T1
     & E2 ==> !T2
     & ---- [ "add", !T1, !T2 ]
  rule ---- [ "add", int, int ]
    => int

  rule E1 * E2
    => E1 ==> !T1
     & E2 ==> !T2
     & ---- [ "mul", !T1, !T2 ]
  rule ---- [ "mul", int, int ]
    => int

  rule E1 <= E2
    => E1 ==> !T1
     & E2 ==> !T2
     & ---- [ "leq", !T1, !T2 ]
  rule ---- [ "leq", int, int ]
    => bool

  rule if E0 then E1 else E2
    => E0 ==> !T0
     & E1 ==> !T1
     & E2 ==> !T2
     & ---- [ "if", !T0, !T1, !T2 ]
  rule ---- [ "if", bool, T, T ]
    => T

  rule E0 E1
    => E0 ==> !T0
     & E1 ==> !T1
     & ---- [ "app", !T0, !T1 ]
  rule ---- [ "app", T1 -> T2, T1 ]
    => T2

  rule lambda X : T1 . E2
    => E2[ T1 / X ] ==> !T2
     & ---- [ "lambda", T1, !T2 ]
  rule ---- [ "lambda", T1,  T2 ]
    => T1 -> T2

  rule mu X : T . E
    => E[T / X] ==> !T'
     & ---- [ "mu", T, !T' ]
  rule ---- [ "mu", T,  T  ]
    => T
endmodule
