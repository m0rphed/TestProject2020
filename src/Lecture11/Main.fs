module Lecture11.Main

open Argu

type CLIArguments =
    | Input of file:string
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Input _ -> "File to interpret."

open Lecture11.Automata
open Lecture10.Regexp
open Lecture10.Automata
open Matrices
open System.Collections.Generic

let processRegexp regexp input =
    let nfa = regexpToNFA regexp

    let res, time = Lecture5.PerfTests.time (fun _ -> recognizeNFA nfa input)

    printfn $"Recognition with graph NFA: %A{res} in %A{time} milliseconds"

    let mtxNFA = nfaToMatrixNFA nfa
    let eclsNFA = epsClosure mtxNFA
    let res, time = Lecture5.PerfTests.time (fun _ -> accept eclsNFA input)
    printfn $"Recognition with matrix NFA: %A{res} in %A{time} milliseconds"
    printfn "------------------------"


[<EntryPoint>]
let main (argv: string array) =
    let parser = ArgumentParser.Create<CLIArguments>(programName = "RegExp interpreter")
    let results = parser.Parse(argv)
    if results.Contains Input
    then
        let file = results.GetResult Input
        let ast = RegexpParser.parseRegexpFromString (System.IO.File.ReadAllText file)
        let _, _, pD = Interpreter.run ast
        printfn "%s" pD.["print"]
    else
        parser.PrintUsage() |> printfn "%s"
    0
