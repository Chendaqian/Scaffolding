// Learn more about F# at http://fsharp.org

open System
open System.IO

open Argu

open Scaffolding

type CLIArguments  =
    | [<MainCommand; ExactlyOnce; First>]Input of string
    | Namespace of string
    | Output of string
with
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Input  _ -> "输入的模板文件"
            | Namespace _ -> "异常类型的名称空间"
            | Output _ -> "输出的文件"

[<EntryPoint>]
let main argv =
    // printfn "argv -> %A" argv
    try
        let parser = ArgumentParser.Create<CLIArguments>(programName = "exmgen.exe")
        let templateArgs = parser.Parse argv

        let ns = templateArgs.GetResult <@ CLIArguments.Namespace @>
        let inputFile = templateArgs.GetResult <@ CLIArguments.Input @>
        let outputFile = templateArgs.GetResult <@ CLIArguments.Output @>

        inputFile |> printfn "Input: %s"
        outputFile |> printfn "Output: %s"
        ns |> printfn "Namespace: %s"

        // ExceptionTemplateParser
        let saveAs (path:string) input =
            File.WriteAllText(path, input)

        inputFile |> Parser.parse
        |> CodeGen.generate ns ["System"]
        // |> printfn "Generated Code ->\n%s"
        // |> printfn "%A"
        // |> sprintf "%A"
        // |> saveAs (Path.Combine(Path.GetDirectoryName(inputFile), "Exceptions.cs"))
        |> saveAs outputFile

    with ex -> printfn " %s" ex.Message

    0 // return an integer exit code