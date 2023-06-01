namespace Scaffolding
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.VisualBasic

module CodeGen =
    open System
    open System.Text.RegularExpressions
    open Microsoft.CodeAnalysis;
    open Microsoft.CodeAnalysis.CSharp
    open Microsoft.CodeAnalysis.CSharp.Syntax
    open Scaffolding.Parser

    type ArgumentOption =
        | Pass
        | Default
        | Const of string

    type ParameterDef = {
        ParamName: string
        ParamType: string
        IsBase: bool
        DefaultValue: ArgumentOption
    }

    type ArgumentDef = {
        ArgumentName: string
        ArgumentType: string
        DefaultValue: ArgumentOption
    }

    type PropertyDef = {
        PropertyName: string
        PropertyType: string
        ReadOnly: bool
    }

    let firstLowerCase input =
        input |> String.mapi (
            fun i ch ->
                match i, ch with
                | 0, ch when ch >= 'A' && ch <= 'Z' -> int ch + 32 |> char
                | _ -> ch
        )

    let firstUpperCase input =
        input |> String.mapi (
            fun i ch ->
                match i, ch with
                | 0, ch when ch >= 'a' && ch <= 'z' -> int ch - 32 |> char
                | _ -> ch
        )

    let syntaxInterpolatedStringText content =
        SyntaxFactory.InterpolatedStringText(
            SyntaxFactory.Token(
                Unchecked.defaultof<SyntaxTriviaList>,
                SyntaxKind.InterpolatedStringTextToken,
                content,
                null,
                Unchecked.defaultof<SyntaxTriviaList>
            )
        ) :> InterpolatedStringContentSyntax

    let syntaxInterpolation content =
        SyntaxFactory.Interpolation(SyntaxFactory.ParseExpression(content)) :> InterpolatedStringContentSyntax

    let parseInterpolatedString (input:string) =
        let rec loop (mode:int, lst: char list, state: char list, result: InterpolatedStringContentSyntax list) =
            match (mode, lst) with
            | 0, hd::tl when hd = '{' ->
                loop(1, tl, [], result @ (String.Concat(state) |> syntaxInterpolatedStringText))
            | 0, [ch] ->
                result @ (String.Concat(state @ ch) |> syntaxInterpolatedStringText)
            | 1, hd::tl when hd = '}' ->
                loop(0, tl, [], result @ (String.Concat(state) |> syntaxInterpolation))
            | 0, hd::tl -> loop(0, tl, state @ hd, result)
            | 1, hd::tl -> loop(1, tl, state @ hd, result)
            | _, [] -> result
            | _, _ -> failwith "Invalid interpolated string."

        loop(0, input |> List.ofSeq, [], [])

    let syntaxQualifiedName (name:string) =
        name.Split('.')
        |> Seq.map (fun name -> SyntaxFactory.IdentifierName(name) :> NameSyntax)
        |> Seq.reduce (fun left right -> SyntaxFactory.QualifiedName(left, (right :?> SimpleNameSyntax)) :> NameSyntax)

    let syntaxNamespace (ns:string) =
        ns |> syntaxQualifiedName |> SyntaxFactory.NamespaceDeclaration

    let syntaxUsing (name:string) =
        name |> syntaxQualifiedName |> SyntaxFactory.UsingDirective

    let syntaxBaseType (name:string) =
        SyntaxFactory.SimpleBaseType(syntaxQualifiedName(name)) :> BaseTypeSyntax

    let syntaxClass (name:string) baseTypes =
        SyntaxFactory.ClassDeclaration(name)
            .AddModifiers(SyntaxFactory.Token(SyntaxKind.PublicKeyword), SyntaxFactory.Token(SyntaxKind.PartialKeyword))
            .AddBaseListTypes(baseTypes |> Seq.map syntaxBaseType |> Seq.toArray)

    let syntaxEqualsValueClause (arg:ParameterDef) =
        match arg.DefaultValue with
        | Default -> SyntaxFactory.EqualsValueClause(SyntaxFactory.DefaultExpression(SyntaxFactory.ParseTypeName(arg.ParamType)))
        | Const(exp) when arg.ParamType = "string" || arg.ParamType = "System.String" ->
            SyntaxFactory.EqualsValueClause(
                SyntaxFactory.InterpolatedStringExpression(
                    SyntaxFactory.Token(SyntaxKind.InterpolatedStringStartToken),
                    SyntaxFactory.List<InterpolatedStringContentSyntax>(
                        exp |> parseInterpolatedString
                    )
                )
            )
        | Const(exp) -> SyntaxFactory.EqualsValueClause(SyntaxFactory.ParseExpression(exp))
        | Pass -> Unchecked.defaultof<EqualsValueClauseSyntax>

    let syntaxParameter (arg:ParameterDef) =
        SyntaxFactory.Parameter(
            Unchecked.defaultof<SyntaxList<AttributeListSyntax>>,
            Unchecked.defaultof<SyntaxTokenList>,
            SyntaxFactory.ParseTypeName(arg.ParamType),
            SyntaxFactory.Identifier(arg.ParamName),
            arg |> syntaxEqualsValueClause)

    let addMember (memberDef: MemberDeclarationSyntax) (classDef:ClassDeclarationSyntax) =
        classDef.AddMembers(memberDef)

    let addCtor (args: ParameterDef seq) (setupBody: ParameterDef seq -> ConstructorDeclarationSyntax -> ConstructorDeclarationSyntax) (classDef:ClassDeclarationSyntax) =
        let ctorDef = SyntaxFactory.ConstructorDeclaration(classDef.Identifier)
        classDef |> addMember (
            ctorDef.AddModifiers(SyntaxFactory.Token(SyntaxKind.PublicKeyword))
                .WithParameterList(
                    SyntaxFactory.ParameterList(
                        SyntaxFactory.SeparatedList<ParameterSyntax>(
                            args |> Seq.map syntaxParameter |> Seq.toArray
                        )
                    )
                ) |> setupBody args
            )

    let syntaxBaseArgument (arg: ArgumentDef) =
        match arg.DefaultValue with
        | Default -> SyntaxFactory.Argument(SyntaxFactory.NameColon(arg.ArgumentName), Unchecked.defaultof<SyntaxToken> ,SyntaxFactory.DefaultExpression(SyntaxFactory.ParseTypeName(arg.ArgumentType)))
        | Const(exp) when arg.ArgumentType = "string" || arg.ArgumentType = "System.String" ->
            SyntaxFactory.Argument(
                SyntaxFactory.NameColon(arg.ArgumentName),
                Unchecked.defaultof<SyntaxToken> ,
                SyntaxFactory.BinaryExpression(
                    SyntaxKind.CoalesceExpression,
                    SyntaxFactory.ParseExpression(arg.ArgumentName),
                    SyntaxFactory.InterpolatedStringExpression(
                        SyntaxFactory.Token(SyntaxKind.InterpolatedStringStartToken),
                        SyntaxFactory.List<InterpolatedStringContentSyntax>(
                            exp |> parseInterpolatedString
                        )
                    )
                )
            )
        | Const(exp) -> SyntaxFactory.Argument(SyntaxFactory.NameColon(arg.ArgumentName), Unchecked.defaultof<SyntaxToken> , SyntaxFactory.ParseExpression(exp))
        | Pass -> SyntaxFactory.Argument(SyntaxFactory.NameColon(arg.ArgumentName), Unchecked.defaultof<SyntaxToken> , SyntaxFactory.ParseExpression(arg.ArgumentName))

    let initializerWithBase (args: ArgumentDef seq) =
        let baseArgs =
            SyntaxFactory.ArgumentList(
                SyntaxFactory.SeparatedList<ArgumentSyntax>(
                    args
                    // |> Seq.where (fun arg -> arg.IsBase)
                    |> Seq.map syntaxBaseArgument
                    |> Seq.toArray
                )
            )
        SyntaxFactory.ConstructorInitializer(SyntaxKind.BaseConstructorInitializer, baseArgs)

    let propsInitWithArgs (args: ParameterDef seq) =
        args
        |> Seq.where (fun arg -> not arg.IsBase)
        |> Seq.map (
            fun arg ->
                SyntaxFactory.ExpressionStatement(
                    SyntaxFactory.AssignmentExpression(
                        SyntaxKind.SimpleAssignmentExpression,
                        SyntaxFactory.MemberAccessExpression(
                            SyntaxKind.SimpleMemberAccessExpression,
                            SyntaxFactory.ThisExpression(),
                            SyntaxFactory.IdentifierName(arg.ParamName |> firstUpperCase)).WithOperatorToken(SyntaxFactory.Token(SyntaxKind.DotToken)),
                        SyntaxFactory.ParseExpression(arg.ParamName)
                    )
                ) :> StatementSyntax
        ) |> Seq.toArray

    let addConst (name:string) (value:string) (classDef:ClassDeclarationSyntax) =
        classDef |> addMember (
            SyntaxFactory.FieldDeclaration(
                SyntaxFactory.VariableDeclaration(
                    SyntaxFactory.ParseTypeName( "string" ),
                    SyntaxFactory.SingletonSeparatedList<VariableDeclaratorSyntax>(
                        SyntaxFactory.VariableDeclarator(
                            SyntaxFactory.Identifier(name),
                            null,
                            SyntaxFactory.EqualsValueClause(
                                SyntaxFactory.LiteralExpression(
                                    SyntaxKind.StringLiteralExpression,
                                    SyntaxFactory.Literal(value)
                                )
                            )
                        )
                    )
                )
            ).AddModifiers(
                SyntaxFactory.Token(SyntaxKind.PrivateKeyword),
                SyntaxFactory.Token(SyntaxKind.ConstKeyword)
            )
        )

    // public T PropertyName { get; private set; }
    let addProperty (propDef: PropertyDef) (classDef:ClassDeclarationSyntax) =
        classDef
        |> addMember (
            SyntaxFactory.PropertyDeclaration(
                SyntaxFactory.ParseTypeName(propDef.PropertyType), propDef.PropertyName
            ).AddModifiers(
                SyntaxFactory.Token(SyntaxKind.PublicKeyword)
            ).AddAccessorListAccessors(
                SyntaxFactory.AccessorDeclaration(
                    SyntaxKind.GetAccessorDeclaration
                ).WithSemicolonToken(
                    SyntaxFactory.Token(SyntaxKind.SemicolonToken)
                ),
                SyntaxFactory.AccessorDeclaration(
                    SyntaxKind.SetAccessorDeclaration
                ).AddModifiers(
                    SyntaxFactory.Token(if propDef.ReadOnly then SyntaxKind.PrivateKeyword else SyntaxKind.PublicKeyword)
                ).WithSemicolonToken(
                    SyntaxFactory.Token(SyntaxKind.SemicolonToken)
                )
            )
        )

    let addClass name baseTypes (setup: ClassDeclarationSyntax -> ClassDeclarationSyntax) (ns: NamespaceDeclarationSyntax) =
        let classDef = syntaxClass name baseTypes
        ns.AddMembers(setup(classDef))

    // let addUsings<'T, 'M when 'T : (member AddUsings: 'M[] -> 'T)> (obj:'T) (items:'M seq) =
    let addUsings imports (ns:NamespaceDeclarationSyntax)=
        let usingDef = imports |> Seq.map syntaxUsing |> Seq.toArray
        ns.AddUsings(usingDef)

    let codeCompileUnit ns imports =
        let compileUnit = SyntaxFactory.CompilationUnit()
        compileUnit.AddMembers(syntaxNamespace ns |> addUsings imports)

    let mapPropertyList props =
            props |> Seq.map (
                fun (propName, propType) ->
                    { PropertyName = propName; PropertyType = propType; ReadOnly = true }
            ) |> Seq.toList

    let inline (|?) (a: 'a option) b = if a.IsSome then a.Value else b

    let mapBaseType = function
        | Some model -> model.Type
        | None -> "System.Exception"

    let withBody body (ctor:ConstructorDeclarationSyntax) =
        ctor.WithBody body

    let withInitializer initializer (ctor:ConstructorDeclarationSyntax) =
        ctor.WithInitializer initializer

    let rec walkthrought f parent =
        parent.Children |> List.collect (
            fun child -> (f child parent)::(walkthrought f { child with Properties = (child.Properties |> List.append <| parent.Properties) |> List.distinct })
        )

    let genBaseArguments child parent =
        [{ ArgumentName = "message"
           ArgumentType = "string"
           DefaultValue = Const(child.Message) };
         { ArgumentName = "innerException"
           ArgumentType = "Exception"
           DefaultValue = Pass }]
        |> List.append <| (
            parent.Properties |> List.map (
                fun (pName, pType) -> { ArgumentName = firstLowerCase pName
                                        ArgumentType = pType
                                        DefaultValue = Pass }
            )
        )

    let genParameters child parent =
        child.Properties
        |> List.map (
            fun (pName, pType) -> { ParamName = firstLowerCase pName
                                    ParamType = pType
                                    IsBase = false
                                    DefaultValue = match child.Values |> Map.tryFind pName with
                                                   | Some(exp) -> Const(exp)
                                                   | None -> Pass }
        ) |> List.append <| [{ ParamName = "message"
                               ParamType = "string"
                               IsBase = true
                               DefaultValue = Default };
                             { ParamName = "innerException"
                               ParamType = "Exception"
                               IsBase = true
                               DefaultValue = Default }]
       |> List.append <| (
            parent.Properties |> List.map (
                fun (pName, pType) -> { ParamName = firstLowerCase pName
                                        ParamType = pType
                                        IsBase = true
                                        DefaultValue = match child.Values |> Map.tryFind pName with
                                                       | Some(exp) -> Const(exp)
                                                       | None -> Default }
            )
        )

    let ns = "UserFramework"
    let imports = ["System"; "System.Runtime.Serialization"]
    let generate ns imports (root: ExceptionNode) =
        root |> walkthrought (
            fun child parent ->
                fun (namesapceDef: NamespaceDeclarationSyntax) ->
                    let baseArguments =  genBaseArguments child parent
                    let parameters = genParameters child parent
                    namesapceDef
                    |> addClass child.Type [parent.Type] (
                        fun classDef ->
                            classDef
                            // |> addConst "DEFAULT_MESSAGE" model.Message
                            |> addCtor parameters (
                                fun args ctor ->
                                    let initializer = baseArguments |> initializerWithBase
                                    let body = parameters |> propsInitWithArgs |> SyntaxFactory.Block
                                    ctor |> withInitializer initializer |> withBody body
                            )
                            |> (
                                fun classDef ->
                                    child.Properties |> mapPropertyList |> Seq.fold (fun c prop -> c |> addProperty prop) classDef
                            )
                    )
        )
        |> List.fold (fun ns gen -> gen ns) (syntaxNamespace ns |> addUsings imports)
        |> (fun compileUnit ->
            compileUnit.NormalizeWhitespace(elasticTrivia = true).ToFullString()
            // compileUnit.Members
            // |> Seq.collect (
            //     fun m ->
            //         m.ChildNodes()
            //         |> Seq.where (fun child -> child.IsKind(SyntaxKind.ConstructorDeclaration))
            //         |> Seq.cast<ConstructorDeclarationSyntax>
            // )
            // |> Seq.map(fun ctor -> ctor.Identifier, ctor.ParameterList.ChildNodes()|>Seq.length)
            // |> Seq.iter (printfn "%A")
        )