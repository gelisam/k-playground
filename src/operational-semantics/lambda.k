require "../syntax/lambda-syntax.k"

module LAMBDA
  imports LAMBDA-SYNTAX

  rule E1 + E2
    => E1 ==> !V1
     & E2 ==> !V2
     & ---- [ "add", !V1, !V2 ]
  rule ---- [ "add",  V1,  V2 ]
    => V1 +Int V2

  rule E1 * E2
    => E1 ==> !V1
     & E2 ==> !V2
     & ---- [ "mul", !V1, !V2 ]
  rule ---- [ "mul",  V1,  V2 ]
    => V1 *Int V2

  rule E1 <= E2
    => E1 ==> !V1
     & E2 ==> !V2
     & ---- [ "leq", !V1, !V2 ]
  rule ---- [ "leq",  V1,  V2 ]
    => V1 <=Int V2

  rule if E0 then E1 else E2
    => E0 ==> !V0
     & ---- [ "if", !V0, E1, E2 ]
  rule ---- [ "if", true, E1, _ ]
    => E1
  rule ---- [ "if", false, _, E2 ]
    => E2

  rule E0 E1
    => E0 ==> !V0
     & E1 ==> !V1
     & ---- [ "app", !V0, !V1 ]
  rule ---- [ "app", lambda X. E2, E1 ]
    => E2[ E1 / X ]
endmodule
