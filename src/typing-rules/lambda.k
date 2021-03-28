require "../syntax/lambda-syntax.k"

module LAMBDA
  imports LAMBDA-SYNTAX

  syntax Hole ::= "hole"
  syntax Type ::= Hole
  syntax KResult ::= Type
  syntax Exp ::= KResult | Hole


  rule _:Int
    => int


  rule HOLE + E2
    => HOLE ~> hole + E2                                                       [heat]
  rule HOLE ~> hole + E2
    => HOLE + E2                                                               [cool]

  rule E1 + HOLE
    => HOLE ~> E1 + hole                                                       [heat]
  rule HOLE ~> E1 + hole
    => E1 + HOLE                                                               [cool]

  rule int + int
    => int


  rule let X1 : T1 = HOLE in E2
    => HOLE ~> let X1 : T1 = hole in E2                                        [heat]
  rule HOLE ~> let X1 : T1 = hole in E2
    => let X1 : T1 = HOLE in E2                                                [cool]

  rule let X1 : T1 = T1:Type in E2
    => E2[ T1 / X1 ]
endmodule
