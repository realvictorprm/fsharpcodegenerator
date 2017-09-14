module Generator 
open System.IO
open System.CodeDom.Compiler
    
type Decl = string * string option

type MemberKind = 
    | AbstractFunction of decl:Decl
    | Function of selfIdentifier:string * decl:Decl * params:Decl[] * content:string[]
    | Property of decl:Decl * get:string * set:string

type Expression =
    | Call of content:string
    | Binding of decl:Decl * content:Expression[]
    | Expression of expressions:Expression[]
    | Array of content:Expression[]

type AST = 
    | AbstractClass of name:string * parameters:Decl[] * members:MemberKind[]
    | Class of name:string * parameters:Decl[] * members:MemberKind[]
    | Interface of name:string * members:Decl[]
    | Module of name:string * childs:AST
    | Binding of decl:Decl * expression : string
    | Alias of decl:Decl
    | Expression of content:AST
    
let pathNamePrefix = "pathTo"
    
let formatDeclWithBrackets (decl:Decl) =
    let name, signatureOption = decl
    match signatureOption with
    | Some s -> sprintf "(%s:%s)" name s
    | None -> sprintf "%s" name
    
let formatDecl (decl:Decl) =
    let name, signatureOption = decl
    match signatureOption with
    | Some s -> sprintf "%s:%s" name s
    | None -> sprintf "%s" name
            
let formatTupleParams parameters =
    let formattedParams = 
        parameters  
        |> Array.map (fun decl -> formatDecl decl)
    sprintf "(%s%s)" 
        (formattedParams 
        |> Array.take (Array.length formattedParams - 1)
        |> String.concat ", ")
        (formattedParams |> Seq.last)

let formatParams parameters =
    let formattedParams = 
        parameters  
        |> Array.map (fun decl -> formatDeclWithBrackets decl)
    sprintf "%s%s" 
        (formattedParams 
        |> Array.take (Array.length formattedParams - 1)
        |> String.concat " ")
        (formattedParams |> Seq.last)

let formatDefinition decl value = 
    let formattedDecl = formatDecl decl
    sprintf """let %s = %s""" formattedDecl value

let formatPathDefinition name value = 
    let pathName = sprintf "%s%s" pathNamePrefix name
    let wrappedValue = sprintf """@"%s" """ value
    formatDefinition (pathName, None) wrappedValue

let formatMethodCall name fParams = sprintf "%s %s" name fParams

let formatTypeAlias decl =
    let name, typeNameOption = decl
    match typeNameOption with
    | Some typeName -> 
        sprintf "type %s = %s" name typeName
    | None -> 
        printfn "WARNING: Incomplete type alias %A" decl
        ""

let formatArray contents =
    [| "|]" |] |> Array.append [|  for content in contents -> " " + content |] |> Array.append [| "[|" |]

type FSharpCodeWriter() as formattedWriter =
    inherit IndentedTextWriter(new StringWriter())
    
    let stringWriter = (formattedWriter.InnerWriter :?> StringWriter)
    let clear () = stringWriter.GetStringBuilder().Clear() |> ignore

    let indent () = formattedWriter.Indent <- formattedWriter.Indent + 1
    let unindent () = formattedWriter.Indent <- formattedWriter.Indent - 1

    let writeAttributes names =
        formattedWriter.Write "[<"
        for name in names do sprintf "%s; " name |> formattedWriter.Write
        formattedWriter.Write ">]"
        
    let beginModule name = 
        indent()
        sprintf "module %s = " name |> formattedWriter.WriteLine

    let beginSingleModule name = 
        sprintf "module %s" name |> formattedWriter.WriteLine

    let endModule () = unindent()

    let writeOpenModuleOrNamespace name = 
        sprintf "open %s" name |> formattedWriter.WriteLine

    let writeAttribute fullName =
        sprintf "[<%s>]" fullName |> formattedWriter.WriteLine

    let writeDefinition decl value = formatDefinition decl value |> formattedWriter.WriteLine 

    let writePathDefinition name value = formatPathDefinition name value |> formattedWriter.WriteLine

    let writeTypeDefinition name paramsOption =
        let formattedParameters = 
            match paramsOption with
            | Some parameters -> 
                if parameters |> Array.length = 0 then
                    "()"
                else formatTupleParams parameters
            | None -> ""
        sprintf "type %s %s =" name formattedParameters |> formattedWriter.WriteLine

    let writeArrayDefinition name contents =
        formattedWriter.WriteLine(sprintf "let %s = " name)
        indent()
        formattedWriter.WriteLine("[|")
        indent()
        contents |> Seq.iter(fun name -> formattedWriter.WriteLine(sprintf """ "%s" """ name))
        unindent()
        formattedWriter.WriteLine("|]")
        unindent()

    let writeAbstractFunction decl =
        let formattedDecl = formatDecl decl
        sprintf "abstract member %s" formattedDecl |> formattedWriter.WriteLine
        
    let writeAbstractMembers abstractMembers =
        for memberDef in abstractMembers do
            writeAbstractFunction memberDef
            formattedWriter.WriteLine()

    let writeMemberFunction selfIdentifier decl fParams (contents:string[]) =
        let formattedDecl = formatDecl decl
        sprintf "member %s.%s %s =" selfIdentifier formattedDecl (fParams |> formatParams)|> formattedWriter.WriteLine
        indent()
        contents |> Array.iter formattedWriter.WriteLine
        unindent()
        
    let writeAbstractClass name (constructorParams:Decl[]) members =
        writeAttribute "AbstractClass"
        writeTypeDefinition name (Some constructorParams)
        indent()
        for memberDef in members do
            formattedWriter.WriteLine()
            match memberDef with
            | AbstractFunction decl as m -> 
                writeAbstractFunction decl
            | Function (selfIdentifier, name, fParams, contents) as def -> 
                writeMemberFunction selfIdentifier name fParams contents
        unindent()

    let writeInterface name (members:Decl[]) =
        writeAttribute "Interface"
        writeTypeDefinition name None
        indent()
        writeAbstractMembers members
        unindent()
        
    let writeTypeAlias decl =
        formatTypeAlias decl |> formattedWriter.WriteLine

    let writeAST moduleName ast dependencies =
        beginSingleModule moduleName
        for dependency in dependencies do writeOpenModuleOrNamespace dependency

        for i in 0..1 do formattedWriter.WriteLine()

        let rec visitAST remainingAST =
            match remainingAST with
            | h :: t ->
                match h with
                | AST.AbstractClass(name, parameters, members) -> 
                    writeAbstractClass name parameters members
                | AST.Interface(name, members) -> 
                    writeInterface name members
                | AST.Binding(decl, expression) -> 
                    writeDefinition decl expression
                | AST.Alias decl -> 
                    writeTypeAlias decl
                | _ as unhandeledType -> 
                    printfn "Unhandeled AST type %A" unhandeledType
                formattedWriter.WriteLine()
                visitAST t
            | _ ->
                ()

        visitAST ast
        
        formattedWriter.Flush()
        formattedWriter.InnerWriter.ToString()
    
    member __.WriteAST moduleName ast dependencies =
        clear ()
        writeAST moduleName ast dependencies
