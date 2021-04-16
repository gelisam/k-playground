require "../syntax/lambda-syntax.k"

module LAMBDA
  imports LAMBDA-SYNTAX

  configuration <Typecheck>
                  <k> $PGM:Exp </k>
                  <ctx> .Map </ctx>
                </Typecheck>

  syntax Hole ::= "hole"
  syntax Type ::= Hole
  syntax KResult ::= "type(" Type ")"                                          [klabel(#type),symbol]
  syntax Exp ::= KResult | Hole


  rule _:Int
    => type( int )


  rule HOLE + E2
    => HOLE ~> hole + E2                                                       [heat]
  rule HOLE ~> hole + E2
    => HOLE + E2                                                               [cool]

  rule type(T1) + HOLE
    => HOLE ~> type(T1) + hole                                                 [heat]
  rule HOLE ~> type(T1) + hole
    => type(T1) + HOLE                                                         [cool]

  rule type(T1) + type(T2)
    => {T1 #Equals int}
  #And {T2 #Equals int}
  #And type(int)


  rule <k> X:KVar => type(T) ... </k>
       <ctx> ... X |-> T ... </ctx>


  rule let X1 = HOLE in E2
    => HOLE ~> let X1 = hole in E2                                             [heat]
  rule HOLE ~> let X1 = hole in E2
    => let X1 = HOLE in E2                                                     [cool]

  rule <k> let X1 = type(T1) in E2
        => E2
       </k>
       <ctx> Gamma:Map
          => Gamma[X1 <- T1]
       </ctx>


  rule <k> \ X1 -> E2
        => E2 ~> Gamma ~> type(?T1 -> hole)
           ...
       </k>
       <ctx> Gamma:Map
          => Gamma[X1 <- ?T1]
       </ctx>

  rule <k> type(T2) ~> Gamma ~> type(T1 -> hole)
        => type(T1 -> T2)
           ...
       </k>
       <ctx> _ => Gamma </ctx>


  rule HOLE E1
    => HOLE ~> hole E1                                                         [heat]
  rule HOLE ~> hole E1
    => HOLE E1                                                                 [cool]

  rule type(TF) HOLE
    => HOLE ~> type(TF) hole                                                   [heat]
  rule HOLE ~> type(TF) hole
    => type(TF) HOLE                                                           [cool]

  rule type(T1 -> T2) type(T1)
    => type(T2)
endmodule
