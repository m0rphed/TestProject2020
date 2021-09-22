module Lecture11.Interpreter

open System.Collections.Generic
open Lecture10 // using Regexp from Lecture 10


let private newDataToConsole = new Event<string>()

let Printed = newDataToConsole.Publish

let private runtimeException = new Event<string>()

let NewRuntimeException = runtimeException.Publish


type vType =
    | RE of Regexp.Regexp<char>
    | Bool of bool
    | Lst of list<int*int>

let rec processRegExp (vDict:Dictionary<_,_>) re =
    match re with
    | AST.RVar v ->
        let data =
            try
                vDict.[v]
            with
            | _ ->
                $"Variable %A{v} is not declared." |> runtimeException.Trigger
                failwith $"Variable %A{v} is not declared."
        match data with
        | RE r -> r
        | Bool _ ->
            $"Variable %A{v} has type bool, but regexp is expected." |> runtimeException.Trigger
            failwithf $"Variable %A{v} has type bool, but regexp is expected."
        | Lst _ ->
            $"Variable %A{v} has type list, but regexp is expected." |> runtimeException.Trigger
            failwithf $"Variable %A{v} has type list, but regexp is expected."
    | AST.RSmb s -> Regexp.RSmb s
    | AST.Alt(l, r) ->
        let l = processRegExp vDict l
        let r = processRegExp vDict r
        Regexp.Alt(l, r)
    | AST.Seq(l, r) ->
        let l = processRegExp vDict l
        let r = processRegExp vDict r
        Regexp.Seq(l, r)
    | AST.Star r ->
        let r = processRegExp vDict r
        Regexp.Star r
    | AST.Opt r ->
        let r = processRegExp vDict r
        Regexp.Alt(Regexp.REps, r)
    | AST.Intersect (l, r) ->
        let l = processRegExp vDict l
        let r = processRegExp vDict r
        Regexp.Intersect(l, r)

let processExpr vDict expr =
    let makeAtm regexp =
        let regexp = processRegExp vDict regexp
        let nfa = Regexp.regexpToNFA regexp
        let mtxNFA = Automata.nfaToMatrixNFA nfa
        Automata.epsClosure mtxNFA
    match expr with
    | AST.FindAll (str, re) ->
        Lst(Automata.findAll (makeAtm re) (str.ToCharArray() |> List.ofArray))
    | AST.IsAcceptable (str, re) ->
        Bool(Automata.accept (makeAtm re) (str.ToCharArray() |> List.ofArray))
    | AST.RegExp re ->
        RE (processRegExp vDict re)

let processStmt (vDict:Dictionary<_,_>) (pDict:Dictionary<string,string>) stmt =
    match stmt with
    | AST.Print v ->
        let data =
            try
                vDict.[v]
            with
            | _ ->
                $"Variable %A{v} is not declared." |> runtimeException.Trigger
                failwithf $"Variable %A{v} is not declared."
        match data with
        | RE r -> pDict.["print"] <- (pDict.["print"] + r.ToString() + "\n")
        | Bool b -> pDict.["print"] <- (pDict.["print"] + b.ToString() + "\n")
        | Lst l -> pDict.["print"] <- (pDict.["print"] + l.ToString() + "\n")
    | AST.VDecl(v,e) ->
        if vDict.ContainsKey v
        then vDict.[v] <- processExpr vDict e
        else vDict.Add(v, processExpr vDict e)
    vDict, pDict

let run ast =
    let vDict = Dictionary<_,_>()
    let pDict = Dictionary<_,_>()
    let varDict = Dictionary<_,_>()
    pDict.Add("print", "")
    let vD, pD = List.fold (fun (d1, d2) -> processStmt d1 d2) (vDict, pDict) ast
    vD, varDict, pDict
