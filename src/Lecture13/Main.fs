module Lecture13.Main

open Lecture13.MyTree

type Adder() =
    let mutable state = 0

    interface IActor<int, int> with
        member this.DoAction(d) =
            state <- state + d
            state

[<EntryPoint>]
let main (argv: string array) =
    let tree =
        BinNode<_>(3,
            BinNode<_>(2,
                BinNode<_>(4,
                    Leaf<_>(5),
                    Leaf<_>(6)
                ),
                Leaf<_>(3)
            ),
            TriNode<_>(7,
                Leaf<_>(2),
                Leaf<_>(4),
                Leaf<_>(4)
            )
        )

    let adder = Adder()
    let visitor = Visitor<_, _>(adder :> IActor<_, _>)
    let res = visitor.Visit tree
    printfn $"Sum = %A{res}"
    
    0
