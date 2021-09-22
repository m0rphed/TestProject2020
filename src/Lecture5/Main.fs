module Lecture5.Main

open Lecture4 // using sorts impl. from Lecture 4

[<EntryPoint>]
let main (argv: string array) =
    let config =
        PerfTests.PerfConfig(10_000, 20_000, 200 * 10_000, false)

    PerfTests.perfTests config List.sort 5 "timingsSystemListSortOut5_release_GC_charging_2.csv"
    PerfTests.perfTests config Sorts.quickSort 5 "timingsCustomListSortOut5_release_GC_charging_2.csv"
    0
