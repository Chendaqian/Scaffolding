namespace Scaffolding

module Parser =
    open System
    open System.IO
    open System.Text.RegularExpressions

    type ExceptionNode = {
        Type: string
        Properties: (string * string) list
        Values: Map<string, string>
        Message: string
        Children: ExceptionNode list
        Level: int
    }

    let (@) lst x = List.append lst [x]

    let split n list =
        let rec loop xs = function
            | (0, ys) | (_, ([] as ys)) -> (List.rev xs), ys
            | (n, x::ys) -> loop (x::xs) (n-1, ys)
        loop [] (n, list)

    let replaceAt i x lst =
        let lst1, lst2 = split i lst
        match lst2 with
        | hd::tl -> List.append lst1 (x::tl)
        | _ -> lst1

    let (@@) (x:Group) (y:Group) =
        Seq.zip (x.Captures |> Seq.cast<Capture> |> Seq.map (fun cap -> cap.Value))
                (y.Captures |> Seq.cast<Capture> |> Seq.map (fun cap -> cap.Value)) |> Seq.toList

    let indentLevelBy wsCount line =
        if String.IsNullOrEmpty(line) then 0
        else
            (line |> Seq.takeWhile (fun c -> c = ' ') |> Seq.length) / wsCount

    let parseProperties (g:Group) =
        g.Captures
        |> Seq.cast<Capture> |> Seq.map (
            fun cap ->
                let segments = cap.Value.Split([| ':' |], StringSplitOptions.RemoveEmptyEntries)
                (segments.[0], segments.[1])
        ) |> List.ofSeq

    let parseValues (g:Group) =
        g.Captures
        |> Seq.cast<Capture> |> Seq.map (
            fun cap ->
                let segments = cap.Value.Split([| '=' |], StringSplitOptions.RemoveEmptyEntries)
                (segments.[0], segments.[1])
        ) |> Map.ofSeq

    let genExceptionNode (input:string) =
        let template = input.Trim()
        let level = (input |> indentLevelBy 4) + 1
        let pattern = @"^(?<TName>[a-zA-Z_][a-zA-Z0-9_]+)(\(" +
                      @"((?<Initializer>(?<PName>[a-zA-Z_][a-zA-Z0-9_]+)\s*=\s*(?<PValue>([\d.]+|\""[\w\W\s]*?\""(?=,)))),?\s*)*" +
                      @"((?<Definition>(?<PName>[a-zA-Z_][a-zA-Z0-9_]+)\s*:\s*(?<PType>[a-zA-Z_][a-zA-Z0-9_.]+)),?\s*)*" +
                      @",?\s*" +
                      @"(\$?\""(?<PMessage>[\w\W\s]*)\"")?" +
                      @"\))?$"
        match Regex.Match(template, pattern) with
        | m when m.Success -> { Type = m.Groups.["TName"].Value
                                Properties = m.Groups.["Definition"] |> parseProperties
                                Values = m.Groups.["Initializer"] |> parseValues
                                Message = m.Groups.["PMessage"].Value
                                Children = []
                                Level = level }
        | _ -> failwith "Invalid template line."

    let readLines (filePath:string) = seq {
        use sr = new StreamReader (filePath)
        while not sr.EndOfStream do
            yield sr.ReadLine ()
    }

    let buildExceptionTree (lines:string list) =
        let rec buildTree root lines =
            buildChildren(lines, root) |> snd
        and buildNode = function
            | line::tl ->
                let node = line |> genExceptionNode
                buildChildren(tl, node)
            | [] -> failwith "Can not build node, lines is empty."
        and buildChildren (lines:string list, parent:ExceptionNode) =
            match lines with
            | [] -> [], parent
            | line::tl ->
                let indentLevel = (line |> indentLevelBy 4) + 1
                if indentLevel > parent.Level then
                    let remaining, model = line::tl |> buildNode
                    buildChildren(remaining, { parent with Children = parent.Children @ model })
                else
                    line::tl, parent

        lines |> buildTree ({ Type = "System.Exception"
                              Properties = []
                              Values = Map.empty
                              Message = ""
                              Children = []
                              Level = 0 })

    let parse filePath =
        readLines filePath
        |> Seq.toList
        |> buildExceptionTree