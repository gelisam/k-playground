require "../syntax/lambda-syntax.k"

module LAMBDA
  imports LAMBDA-SYNTAX

  syntax Exp ::= "hole" | Type
  syntax KResult ::= Type


  rule _:Int
    => int

  rule E1:Exp + E2:Exp
    => E1 ~> hole + E2

  rule T1:Type ~> hole + E2:Exp
    => E2 ~> T1 + hole

  rule T1:Type ~> hole + E2:Exp
    => E2 ~> T1 + hole

  rule T2:Type ~> T1:Type + hole
    => T1 + T2

  rule int + int
    => int
endmodule
