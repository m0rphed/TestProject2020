module Lecture5.PerfTests

open System

/// Config of performance test
/// - forceGC: turn on/off immediate garbage collection of all generations
[<Struct>]
type PerfConfig =
    val fromNum: int
    val step: int
    val toNum: int
    val forceGC: bool

    new(_from, _step, _to, forceGC) =
        { fromNum = _from
          step = _step
          toNum = _to
          forceGC = forceGC }

/// Generates random list of length = n
let genRandomList n =
    let rand = System.Random()
    List.init n (fun _ -> rand.Next())

/// Calculates the execution time of the function
let time func =
    let timer = System.Diagnostics.Stopwatch()
    timer.Start()
    let res = func ()
    let time = timer.ElapsedMilliseconds
    res, time

let perfTests (perfConfig: PerfConfig) sortFun cycles file =
    let iterations =
        [ perfConfig.fromNum .. perfConfig.step .. perfConfig.toNum ]

    let timings = Array.zeroCreate iterations.Length
    let mutable counter = 0

    for n in iterations do
        let randomData = genRandomList n
        let times = Array.zeroCreate cycles

        for cycleNum in 0 .. cycles - 1 do
            if perfConfig.forceGC then GC.Collect()
            let _res, time = time (fun _ -> sortFun randomData)
            if perfConfig.forceGC then GC.Collect()
            times.[cycleNum] <- string time
            printfn $"Measured: len = %A{n}, iteration = %A{cycleNum}, time = %A{time}"

        timings.[counter] <- (string n) + "," + (String.concat ", " times)
        counter <- counter + 1

    System.IO.File.WriteAllLines(file, timings)
