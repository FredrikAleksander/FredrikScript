namespace FredrikScript
open FParsec

module Parser =
    open Ast

    type UserState = unit
    type Parser<'a> = Parser<'a, UserState>

    let contextInfo (pos : Position) : ContextInfo =
        ContextInfo(pos.StreamName, pos.Index, pos.Line, pos.Column)

    let pContextInfo : Parser<ContextInfo> = getPosition |>> contextInfo

    ///#region Keywords
    let pKeywordVoid : Parser<string> = pstring "void"
    let pKeywordClass : Parser<string> = pstring "class"
    let pKeywordStruct : Parser<string> = pstring "struct"
    let pKeywordInterface : Parser<string> = pstring "interface"
    let pKeywordNamespace : Parser<string> = pstring "namespace"
    let pKeywordConstructor : Parser<string> = pstring "constructor"
    let pKeywordEnum : Parser<string> = pstring "enum"
    let pKeywordUsing : Parser<string> = pstring "using"
    let pKeywordBraceStart : Parser<string> = pstring "{"
    let pKeywordBraceEnd : Parser<string> = pstring "}"
    let pKeywordStatic : Parser<string> = pstring "static"
    let pKeywordPublic : Parser<string> = pstring "public"
    let pKeywordPrivate : Parser<string> = pstring "private"
    let pKeywordProtected : Parser<string> = pstring "protected"
    let pKeywordInternal : Parser<string> = pstring "internal"
    let pKeywordColon : Parser<string> = pstring ":"
    let pKeywordSemicolon : Parser<string> = pstring ";"
    let pKeywordEquals : Parser<string> = pstring "="
    let pKeywordComma : Parser<string> = pstring ","
    let pKeywordPeriod : Parser<string> = pstring "."
    let pKeywordParensStart : Parser<string> = pstring "("
    let pKeywordParensEnd : Parser<string> = pstring ")"
    let pKeywordReturn : Parser<string> = pstring "return"
    let pKeywordExtern : Parser<string> = pstring "extern"
    let pKeywordStar : Parser<string> = pstring "*"
    let pKeywordBracketStart : Parser<string> = pstring "["
    let pKeywordBracketEnd : Parser<string> = pstring "]"
    let pKeywordBrackets : Parser<string> = pchar '[' >>. spaces >>. pchar ']' >>% "[]"
    let pKeywordVar : Parser<string> = pstring "var"
    let pKeywordWhile : Parser<string> = pstring "while"
    let pKeywordDo : Parser<string> = pstring "do"
    let pKeywordForEach : Parser<string> = pstring "foreach"
    let pKeywordFor : Parser<string> = pstring "for"
    let pKeywordIf : Parser<string> = pstring "if"
    let pKeywordElse : Parser<string> = pstring "else"
    let pKeywordBreak : Parser<string> = pstring "break"
    let pKeywordContinue : Parser<string> = pstring "continue"
    let pKeywordIn : Parser<string> = pstring "in"
    let pKeywordElseIf : Parser<string> = pKeywordElse >>? spaces1 >>? pKeywordIf >>% "else if"
    let pKeywordNew : Parser<string> = pstring "new"
    ///#endregion Keywords
    ///#region Forward References
    let ((pExpression : Parser<Expression>), pExpressionImpl) = createParserForwardedToRef()
    let ((pStatement : Parser<Statement>), pStatementImpl) = createParserForwardedToRef()
    ///#endregion Forward References
    ///#region Helpers
    let pCharLiteral : Parser<char> =
        let normalChar = satisfy (fun c -> c <> '\\' && c <> '\'' && c <> '\n' && c <> '\r' && c <> '\t')
        let unescape c = match c with
                         | 'n' -> '\n'
                         | 'r' -> '\r'
                         | 't' -> '\t'
                         | c   -> c
        let escapedChar = pstring "\\" >>. (anyOf "\\nrt'" |>> unescape)
        between (pstring "\"") (pstring "\"") (normalChar <|> escapedChar) <?> "char literal"

    let pStringLiteral : Parser<string> =
        let normalChar = satisfy (fun c -> c <> '\\' && c <> '"')
        let unescape c = match c with
                         | 'n' -> '\n'
                         | 'r' -> '\r'
                         | 't' -> '\t'
                         | c   -> c
        let escapedChar = pstring "\\" >>. (anyOf "\\nrt\"" |>> unescape)
        between (pstring "\"") (pstring "\"")
                (manyChars (normalChar <|> escapedChar)) <?> "string literal"

    let pIdentifier : Parser<string> =
        let pleading = satisfy (fun x -> x = '_' || (x >= 'a' && x <= 'z') || (x >= 'A' && x <= 'Z'))
        let pany     = satisfy (fun x -> x = '_' || (x >= 'a' && x <= 'z') || (x >= 'A' && x <= 'Z') || (x >= '0' && x <= '9'))
        let pname    = many1Chars2 pleading pany

        let validate (p : Parser<string>) : Parser<string> =
            let reservedKeyword = 
                [
                "return";"var";"object";"string"
                "byte";"sbyte";"short";"ushort"
                "int";"uint";"long";"ulong";"float"
                "double";"null";"void";"class";
                "struct";"interface";"enum";
                "public";"protected";"private";
                "internal";"namespace";"using";
                "static";"extern";"true";"false";
                "for";"foreach";"if";"else";
                "break";"while";"do";"goto";
                "continue";"in";"out";"ref";
                "new"
                ]

            fun stream ->
                let reply = p stream
                if reply.Status = Ok then
                    if List.contains reply.Result reservedKeyword then Reply(ReplyStatus.Error, messageError (sprintf "Keyword is invalid as identifier '%s'" reply.Result))
                    else reply
                else
                    reply
        validate pname <?> "identifier"

    let pIdentifierM : Parser<string> =
        let ptrail   = pKeywordPeriod >>. pIdentifier

        let p = pIdentifier .>>. many ptrail |>> (fun (x,y) -> 
            let r = y |> List.fold (fun i j ->
                    i + "." + j
                    ) x
            r)
        p <?> "identifier"

    let pName : Parser<Name> =
        pIdentifier |>> Name <?> "name"
    let pNameM : Parser<Name> =
        pIdentifierM |>> Name <?> "name"

    let pTypeName : Parser<TypeName> =
        let validate (p : Parser<_>) = 
            let reservedKeyword = 
                ["return";"var";"namespace";"struct";"class";
                "interface";"enum";"using";"static";"extern";
                "constructor";"true";"false";"for";"foreach";"if";"else";
                "break";"while";"do";"goto";"continue";"in";"out";"ref";"new"]

            fun stream ->
                let reply = p stream
                if reply.Status = Ok then
                    if List.contains reply.Result reservedKeyword then Reply(ReplyStatus.Error, messageError (sprintf "Keyword '%s' is not valid as an identifier" reply.Result))
                    else reply
                else
                    reply
    
        let pleading = satisfy (fun x -> x = '_' || (x >= 'a' && x <= 'z') || (x >= 'A' && x <= 'Z'))
        let pany     = satisfy (fun x -> x = '_' || (x >= 'a' && x <= 'z') || (x >= 'A' && x <= 'Z') || (x >= '0' && x <= '9'))
        let pname'    = many1Chars2 pleading pany
        let pname = validate pname'
        let ptrail   = pKeywordPeriod >>. pname
        let p = pname .>>. many ptrail |>> (fun (x,y) -> 
            let r = y |> List.fold (fun i j ->
                    i + "." + j
                    ) x
            TypeName(r, []))

        let pVoid = pKeywordVoid >>. opt (pKeywordStar)
                    |>> fun s ->
                        match s with
                        | Some _ -> TypeName ("void", [Pointer])
                        | _ -> TypeName ("void", [])
        (pVoid <|> p) .>>. (opt (many (attempt pKeywordBrackets |>> fun _ -> Rank.Array))) |>> (fun (TypeName (it ,ir), r) ->
        match r with
        | Some ra -> TypeName(it, ir @ ra)
        | _ -> TypeName(it, ir))
        <?> "typename"
    let pParameter : Parser<Parameter> = 
        pName .>> spaces .>> pKeywordColon .>> spaces .>>. pTypeName .>> spaces |>> Parameter
    let pParameters : Parser<Parameter list> = sepBy pParameter (pKeywordComma .>> spaces)
    let pInvocation : Parser<Expression list> = 
        let pArgs : Parser<Expression list> = sepBy pExpression (pKeywordComma .>> spaces)
        pKeywordParensStart >>. spaces >>. pArgs .>> pKeywordParensEnd .>> spaces
    let pTypeAnnotation = pKeywordColon >>. spaces >>. pTypeName .>> spaces
    let pMethodSignature : Parser<Parameter list * TypeName> =
        let pMethodParametersStart = pKeywordParensStart >>. spaces
        let pMethodParametersEnd = pKeywordParensEnd >>. spaces >>. opt (pTypeAnnotation .>> spaces)
        pMethodParametersStart >>. pParameters .>>. pMethodParametersEnd
        |>> fun (parameters, typeAnnotationS) ->
            match typeAnnotationS with
            | Some tn -> (parameters, tn)
            | _ -> (parameters, TypeName("void", []))
    let pStorageClass : Parser<StorageClass> =
        let pExtern = pKeywordExtern >>? spaces1 >>. pStringLiteral .>> spaces1 |>> (fun d -> Extern d)
        let pStatic = pKeywordStatic >>? spaces1 |>> (fun _ -> Static)

        let p = opt (pStatic <|> pExtern)
            |>> function
            | Some d -> d
            | _ -> Instance
        p <?> "storage class"
    let pAccessModifier a : Parser<AccessModifier> =
        let p =
            List.reduce (<|>) a |>> function
                        | "public" -> Public
                        | "protected" -> Protected
                        | "private" -> Private
                        | "internal" -> Internal
                        | _ -> failwith ""
        opt (p .>> spaces1) .>> spaces |>> (fun x ->
            match x with
            | Some r -> r
            | _ -> Private) <?> "access modifier"
    let pWhitespaceSymbolOrEof : Parser<unit> = 
        (satisfy (fun x -> 
            System.Char.IsWhiteSpace(x) || (System.Char.IsDigit(x) || System.Char.IsLetter(x)) = false
        ) >>% ()) <|> eof

    let pCodeBlock : Parser<Statement list> =
        pKeywordBraceStart >>. spaces >>. manyTill pStatement pKeywordBraceEnd .>> spaces
    ///#endregion Helpers
    ///#region Expressions
    type private PNewObjectOrArray = PObject of Expression list | PArray of Expression
    let pNewExpression =
        let pLeading = pKeywordNew >>? spaces1 >>. pTypeName .>> spaces
        let pArray = pKeywordBracketStart >>. spaces >>. pExpression .>> pKeywordBracketEnd .>> spaces |>> PArray
        let pObject = pInvocation |>> PObject
        let p = pLeading .>>. (pArray <|> pObject)
        let validate (p : Parser<TypeName * PNewObjectOrArray>) : Parser<Expression> =
            (fun stream ->
                let reply = p stream
                if(reply.Status = Error) then Reply(reply.Status, reply.Error)
                else
                    let ((typename : TypeName), (objectOrArray : PNewObjectOrArray)) = reply.Result
                    let (TypeName (n,r)) = typename
                    match objectOrArray with
                    | PObject exprs ->
                        if (List.isEmpty r) then
                            Reply(NewObject(typename, exprs))
                        else
                            Reply(Error, messageError "Arrays cannot be constructed as objects")
                    | PArray expr ->
                        let newTypeName = TypeName(n, Array::r)
                        Reply(NewArray(newTypeName, expr)))
        validate p
    let pScopedExpression : Parser<Expression> =
        between (pstring "(" >>. spaces) (pstring ")" >>. spaces) pExpression .>> spaces
    let pBooleanExpression : Parser<Expression> =
        (pstring "true" <|> pstring "false") .>>? followedBy pWhitespaceSymbolOrEof .>> spaces
        |>> fun s ->
            match s with 
            | "true" -> Boolean (true)
            | _ -> Boolean (false)
    let pStringExpression : Parser<Expression> =
        pStringLiteral .>>? followedBy pWhitespaceSymbolOrEof .>> spaces |>> String <?> "string"
    let pCharExpression : Parser<Expression> =
        pCharLiteral .>>? followedBy pWhitespaceSymbolOrEof .>> spaces |>> Char <?> "character"
    let pNumberExpression : Parser<Expression> =
        let numberFormat =     
                NumberLiteralOptions.AllowMinusSign
                ||| NumberLiteralOptions.AllowFraction
                ||| NumberLiteralOptions.AllowExponent
        let pLongSuffix = satisfy (fun x -> x = 'l' || x = 'L') |>> fun _ -> 'L'
        let pFloatSuffix = satisfy (fun x -> x = 'f' || x = 'F') |>> fun _ -> 'F'

        let validate (p : Parser<(NumberLiteral * char option)>) : Parser<Expression> =
            fun stream ->
                let reply = attempt p stream
                if reply.Status = Error then Reply(reply.Status, reply.Error)
                else
                    let (nl, suffix) = reply.Result
                    match suffix with
                    | Some 'F' ->
                        if nl.IsInteger then Reply(ReplyStatus.Error, messageError "Expected floating point number")
                        else Reply(Float (float32 nl.String))
                    | Some 'L' ->
                        if nl.IsInteger then Reply(Long (int64 nl.String))
                        else Reply(ReplyStatus.Error, messageError "Expected integer number")
                    | _ ->
                        if nl.IsInteger then Reply(Integer (int32 nl.String))
                        else Reply(Double (double nl.String))

        validate (numberLiteral numberFormat "number" .>>. opt (pLongSuffix <|> pFloatSuffix) .>>? followedBy pWhitespaceSymbolOrEof .>> spaces)
        <?> "number"
    let pSymbolExpression =
        let pIdent =
            let validate (p : Parser<_>) = 
                let reservedKeyword = 
                    ["return";"var";"namespace";"struct";"class";
                    "interface";"enum";"using";"static";"extern";
                    "constructor";"true";"false";"void*";"void";
                    "for";"foreach";"if";"else";"break";"while";
                    "do";"goto";"continue";"in";"out";"ref";"new"]
                fun stream ->
                    let reply = (attempt p) stream
                    if reply.Status = Ok then
                        if List.contains reply.Result reservedKeyword then Reply(ReplyStatus.Error, messageError (sprintf "Keyword '%s' is not valid as an identifier" reply.Result))
                        else reply
                    else
                        reply
    
            let pleading = satisfy (fun x -> x = '_' || (x >= 'a' && x <= 'z') || (x >= 'A' && x <= 'Z'))
            let pany     = satisfy (fun x -> x = '_' || (x >= 'a' && x <= 'z') || (x >= 'A' && x <= 'Z') || (x >= '0' && x <= '9'))
            let pname'    = many1Chars2 pleading pany
            let pname = validate pname' .>>? followedBy pWhitespaceSymbolOrEof .>> spaces
            pname |>> (fun s -> Expression.Symbol s) <?> "symbol"
        pIdent
    let chainIndexExpressions expr indices =
        let head = Index(expr, List.head indices)
        let newList = List.skip 1 indices
        let folder state index = 
            Index(state, index)
        List.fold folder head newList
    type private PIndexOrInvocation = PIndex of Expression list | PInvocation of Expression list
    let pLeafExpression =
        let pExtendedExpression =
            let pIndex = many1 (between pKeywordBracketStart pKeywordBracketEnd (spaces >>. pExpression .>> spaces)) |>> PIndex
            let pInvoke = pInvocation |>> PInvocation

            pSymbolExpression .>> spaces .>>. opt ((pIndex <|> pInvoke) .>> spaces)
            |>> fun (sym, indexOrInvokeS) ->
                match indexOrInvokeS with
                | Some (PIndex indices) ->
                    chainIndexExpressions sym indices
                | Some (PInvocation args) ->
                    Expression.Call(sym, args)
                | _ -> sym
        let pMemberSegment =
            pIdentifier .>> spaces .>>. opt (between pKeywordBracketStart pKeywordBracketEnd (spaces >>. pExpression .>> spaces) .>> spaces) 
        let pMember = 
            let pLeading = pKeywordPeriod >>. spaces >>. pMemberSegment
            many1 pLeading .>> spaces .>>. opt (pInvocation .>> spaces)
        let pLeaf =
            let pm instance (membr,index) =
                let m = Expression.Member(instance, membr)
                match index with
                | Some i -> Expression.Index(m, i)
                | _ -> m
            (pScopedExpression <|> pBooleanExpression <|> pStringExpression <|> pCharExpression <|> pNumberExpression <|> pNewExpression <|> pExtendedExpression) .>>. opt pMember
            |>> fun (expr, (membrS)) ->
                match membrS with
                | Some (membr, invocationS) ->
                    let head =  pm expr (List.head membr)
                    let newList = List.skip 1 membr
                    let mm = List.fold pm head newList

                    match invocationS with
                    | Some invocation -> Expression.Call(mm, invocation)
                    | _ -> mm
                | _ -> expr
            
        pLeaf
    ///#endregion Expressions
    ///#region Operators
    type Assoc = Associativity
    let opp = new OperatorPrecedenceParser<Expression, _, _>()        
    opp.AddOperator(PostfixOperator("++", spaces, 16, true, fun x -> PostfixIncrement(x)))
    opp.AddOperator(PostfixOperator("--", spaces, 16, true, fun x -> PostfixDecrement(x)))
    opp.AddOperator(PrefixOperator("++", spaces, 15, true, fun x -> PrefixIncrement(x)))
    opp.AddOperator(PrefixOperator("--", spaces, 15, true, fun x -> PrefixDecrement(x)))
    opp.AddOperator(PrefixOperator("+", spaces, 15, true, fun x -> UnaryPlus(x)))
    opp.AddOperator(PrefixOperator("-", spaces, 15, true, fun x -> UnaryMinus(x)))
    opp.AddOperator(PrefixOperator("!", spaces, 15, true, fun x -> Not(x)))
    opp.AddOperator(PrefixOperator("~", spaces, 15, true, fun x -> BitwiseNot(x)))
    opp.AddOperator(InfixOperator("*", spaces, 13, Assoc.Left, fun x y -> Multiply(x,y)))
    opp.AddOperator(InfixOperator("/", spaces, 13, Assoc.Left, fun x y -> Divide(x, y)))
    opp.AddOperator(InfixOperator("%", spaces, 13, Assoc.Left, fun x y -> Modulus(x,y)))
    opp.AddOperator(InfixOperator("+", spaces, 12, Assoc.Left, fun x y -> Add(x,y)))
    opp.AddOperator(InfixOperator("-", spaces, 12, Assoc.Left, fun x y -> Subtract(x,y)))
    opp.AddOperator(InfixOperator("<<", spaces, 11, Assoc.Left, fun x y -> BitshiftLeft(x,y)))
    opp.AddOperator(InfixOperator(">>", spaces, 11, Assoc.Left, fun x y -> BitshiftRight(x,y)))
    opp.AddOperator(InfixOperator("<", spaces, 10, Assoc.Left, fun x y -> LowerThan(x,y)))
    opp.AddOperator(InfixOperator("<=", spaces, 10, Assoc.Left, fun x y -> LowerThanEquals(x,y)))
    opp.AddOperator(InfixOperator(">", spaces, 10, Assoc.Left, fun x y -> GreaterThan(x,y)))
    opp.AddOperator(InfixOperator(">=", spaces, 10, Assoc.Left, fun x y -> GreaterThanEquals(x,y)))
    opp.AddOperator(InfixOperator("==", spaces, 9, Assoc.Left, fun x y -> Equals(x, y)))
    opp.AddOperator(InfixOperator("!=", spaces, 9, Assoc.Left, fun x y -> NotEqual(x,y)))
    opp.AddOperator(InfixOperator("&", spaces, 8, Assoc.Left, fun x y -> BitwiseAnd(x,y)))
    opp.AddOperator(InfixOperator("^", spaces, 7, Assoc.Left, fun x y -> BitwiseXor(x,y)))
    opp.AddOperator(InfixOperator("|", spaces, 6, Assoc.Left, fun x y -> BitwiseOr(x,y)))
    opp.AddOperator(InfixOperator("&&", spaces, 5, Assoc.Left, fun x y -> And(x,y)))
    opp.AddOperator(InfixOperator("||", spaces, 4, Assoc.Left, fun x y -> Or(x,y)))
    opp.AddOperator(TernaryOperator("?", spaces, ":", spaces, 3, Assoc.Right, fun x y z -> Ternary(x, y, z)))
    opp.AddOperator(InfixOperator("=", spaces, 3, Assoc.Right, fun x y -> Assignment(x,y)))
    opp.AddOperator(InfixOperator("+=", spaces, 3, Assoc.Right, fun x y -> AddAssignment(x,y)))
    opp.AddOperator(InfixOperator("-=", spaces, 3, Assoc.Right, fun x y -> SubtractAssignment(x,y)))
    opp.AddOperator(InfixOperator("*=", spaces, 3, Assoc.Right, fun x y -> MultiplyAssignment(x,y)))
    opp.AddOperator(InfixOperator("/=", spaces, 3, Assoc.Right, fun x y -> DivideAssignment(x,y)))
    opp.AddOperator(InfixOperator("%=", spaces, 3, Assoc.Right, fun x y -> ModulusAssignment(x,y)))
    opp.AddOperator(InfixOperator("<<=", spaces, 3, Assoc.Right, fun x y -> BitshiftLeftAssignment(x,y)))
    opp.AddOperator(InfixOperator(">>=", spaces, 3, Assoc.Right, fun x y -> BitshiftRightAssignment(x,y)))
    opp.AddOperator(InfixOperator("&=", spaces, 3, Assoc.Right, fun x y -> BitwiseAndAssignment(x,y)))
    opp.AddOperator(InfixOperator("^=", spaces, 3, Assoc.Right, fun x y -> BitwiseXorAssignment(x,y)))
    opp.AddOperator(InfixOperator("|=", spaces, 3, Assoc.Right, fun x y -> BitwiseOrAssignment(x,y)))
    opp.TermParser <- pLeafExpression
    do pExpressionImpl := opp.ExpressionParser
    ///#endregion Operators
    ///#region Statements
    let pBreakStatement =
        pKeywordBreak >>. followedBy pWhitespaceSymbolOrEof >>. spaces >>% Break
        <?> "break statement"
    let pContinueStatement =
        pKeywordContinue >>. followedBy pWhitespaceSymbolOrEof >>. spaces >>% Continue
        <?> "continue statement"
    let pVarStatement =
        let pVarLeading = pKeywordVar >>? spaces1 >>. pIdentifier .>> spaces
        let pVarTrailing = opt pTypeAnnotation .>>. opt (pKeywordEquals >>. spaces >>. pExpression .>> followedBy pWhitespaceSymbolOrEof)

        pVarLeading .>>. pVarTrailing 
        |>> (fun (i, (t, d)) ->
            match d with
                | Some e -> VariableDeclaration(i, t, Some e)
                | _ -> VariableDeclaration(i, t, None))
        <?> "var statement"
    let pReturnStatement =
        pKeywordReturn >>. (opt (spaces1 >>? pExpression) <|> (followedBy pWhitespaceSymbolOrEof >>. spaces >>% None))
        |>> Return <?> "return statement"
    let pWhileStatement =
        pKeywordWhile >>. spaces >>. pKeywordParensStart >>. spaces >>. pExpression .>> pKeywordParensEnd .>> spaces .>>. (pCodeBlock <|> (pStatement |>> fun s -> [s]))
        |>> While <?> "while statement"
    let pDoWhileStatement =
        pKeywordDo >>. spaces >>. (pCodeBlock <|> (pStatement |>> fun s -> [s])) .>> pKeywordWhile .>> spaces .>> pKeywordParensStart .>> spaces .>>. pExpression .>> pKeywordParensEnd .>> spaces
        |>> DoWhile <?> "do statement"
    let pEmptyStatement : Parser<Statement> = pstring ";" >>. spaces >>% Empty
    let pForEachStatement =
        pKeywordForEach >>. spaces >>. pKeywordParensStart >>. spaces >>. pKeywordVar >>. spaces1 >>. pName .>> spaces1 .>> pKeywordIn .>> spaces1 .>>. pExpression .>>
        pKeywordParensEnd .>> spaces .>>. (pCodeBlock <|> (pStatement |>> fun s -> [s]))
        |>> (fun (((name), expr), statements) -> ForEach (name, expr, statements))
        <?> "foreach statement"
    let pExpressionStatement =
        pExpression |>> Statement.Expression
        <?> "expression statement"
    let pForStatement =
        let pInitStatement = opt (pVarStatement <|> pExpressionStatement) .>> pKeywordSemicolon .>> spaces
        let pExpr = opt pExpression
        pKeywordFor >>. spaces >>. pKeywordParensStart >>. spaces >>. pInitStatement .>>. pExpr .>> pKeywordSemicolon .>> spaces .>>. pExpr .>> pKeywordParensEnd .>> spaces .>>. (pCodeBlock <|> (pStatement |>> fun s -> [s]))
        |>> (fun (((initStatement, conditionStatement), afterStatement), statements) ->
            let is = match initStatement with
                     | Some is' -> is'
                     | _ -> Empty
            let cs = match conditionStatement with
                     | Some cs' -> cs'
                     | _ -> Nop
            let afs = match afterStatement with
                      | Some afs' -> afs'
                      | None -> Nop
            For(is, cs, afs, statements))
        <?> "for statement"
    let pIfStatement =
        let pElseIfStatement = pKeywordElseIf >>. spaces >>. pKeywordParensStart >>. spaces >>. pExpression .>> pKeywordParensEnd .>> spaces .>>. (pCodeBlock <|> (pStatement |>> fun s -> [s]))
        let pElseStatement = pKeywordElse >>. spaces >>. (pCodeBlock <|> (pStatement |>> fun s -> [s]))
        let pIfStatement' = pKeywordIf >>. spaces >>. pKeywordParensStart >>. spaces >>. pExpression .>> pKeywordParensEnd .>> spaces .>>. (pCodeBlock <|> (pStatement |>> fun s -> [s]))
        pIfStatement' .>>. many pElseIfStatement .>>. opt pElseStatement |>> (fun (((ifExpression, ifStatement), elseIfStatementsM), elseStatementS) -> If (ifExpression, ifStatement, elseIfStatementsM, elseStatementS)) <?> "if statement"
    do pStatementImpl :=
        let needTerminator = 
            pVarStatement <|> pReturnStatement <|> pBreakStatement <|> pContinueStatement  <|> pExpressionStatement
        let noTerminatorNeeded = 
            pWhileStatement <|> pDoWhileStatement <|> pForEachStatement <|> pForStatement <|> pIfStatement
        noTerminatorNeeded <|> (needTerminator .>> pKeywordSemicolon .>> spaces)
    ///#endregion
    ///#region Types
    type private PTypeDeclaration = PEnum of ContextInfo * Name * EnumValue list | PInterface of ContextInfo * Name * TypeName list * InterfaceMember list | PStruct of ContextInfo * Name * TypeMember list | PClass of ContextInfo * Name * TypeName list * TypeMember list
    let pInheritance =
        let pTrail = pKeywordComma >>. spaces >>. pTypeName .>> spaces
        let validate (p : Parser<TypeName list>) : Parser<TypeName list> =
            fun stream ->
                let reply = attempt p stream
                if reply.Status = Error then Reply(reply.Status, reply.Error)
                else
                    let typeIsArrayOrPointer (t : TypeName) =
                        let (TypeName (typeName, rank)) = t
                        (List.isEmpty rank) = false
                    let typeIsPrimitive (t : TypeName) =
                        let (TypeName (typeName, rank)) = t
                        List.contains typeName [
                            "bool";
                            "char";
                            "byte";
                            "sbyte";
                            "short";
                            "ushort";
                            "int";
                            "uint";
                            "long";
                            "ulong";
                            "float";
                            "double";
                            "string";
                            "void"
                        ]
                    let typeIsObject (t : TypeName) =
                        let (TypeName (typeName, rank)) = t
                        typeName = "object"
            
                    let anyTypeIsArray = List.exists typeIsArrayOrPointer reply.Result
                    let anyTypeIsPrimitive = List.exists typeIsPrimitive reply.Result
                    let anyTypeIsObject = List.exists typeIsObject reply.Result

                    if anyTypeIsArray then Reply(Error, messageError "Cannot inherit from arrays or pointers")
                    elif anyTypeIsPrimitive then Reply(Error, messageError "Cannot inherit from primitive")
                    elif anyTypeIsObject then Reply(Error, messageError "Inheriting from object is implicit")
                    else reply

        validate (pKeywordColon >>. spaces >>. pTypeName .>> spaces .>>. (many (pTrail)) |>> fun (a,b) -> a::b)
    let private pEnum = 
        let pa = pAccessModifier [pKeywordInternal;pKeywordPrivate;pKeywordPublic]
        let pEnumValue = pKeywordEquals >>. spaces >>. pint32
        let pEnumMember = pName .>> spaces .>>. opt pEnumValue .>> spaces |>> EnumValue
        let pTrailing = pKeywordComma >>. spaces >>. pEnumMember
        let pEnumMembers = pEnumMember .>>. manyTill pTrailing pKeywordBraceEnd |>> (fun (x,y) -> x::y)
        let pMap ((x, c), y) =
            PEnum (c,x,y)
        pKeywordEnum >>. spaces1 >>. pName .>>. pContextInfo .>> spaces .>> pKeywordBraceStart .>> spaces .>>. pEnumMembers .>> spaces
        |>> pMap <?> "enum"
    let private pInterface =
        let pInterfaceMethod: Parser<InterfaceMember> =
            let p = pName .>> spaces .>>. pMethodSignature .>> pKeywordSemicolon .>> spaces |>> (fun (x,(y,z)) ->
                Method (x, y, z))
            p <?> "method"
        let pMethods : Parser<InterfaceMember list> = 
            manyTill pInterfaceMethod pKeywordBraceEnd
        let pMap (((x,c), y),z) =
            match y with
            | Some y' -> PInterface(c, x, y', z)
            | _ -> PInterface(c, x, [], z)
        pKeywordInterface >>. spaces1 >>. pName .>>. pContextInfo .>> spaces .>>. opt pInheritance .>> pKeywordBraceStart .>> spaces .>>. pMethods .>> spaces
        |>> pMap
        <?> "interface"

    type private PMethodDeclarationOrDefinition =
        | Declaration of Statement list
        | Definition
    type private PFieldOrMethod =
        | Field of TypeName
        | Method of Parameter list * TypeName * PMethodDeclarationOrDefinition
    type private PMember =
        | FieldOrMethod of StorageClass * ContextInfo * Name * PFieldOrMethod
        | Constructor of ContextInfo * Parameter list * Statement list
    let private pTypeMember =
        let pConstructor : Parser<PMember> =
            pKeywordConstructor >>. pContextInfo .>> spaces .>> pKeywordParensStart .>> spaces .>>. pParameters .>> pKeywordParensEnd .>> spaces .>>. pCodeBlock .>> spaces 
            |>> (fun ((ctx, parameters), statements) ->
                PMember.Constructor(ctx, parameters, statements)) <?> "constructor"
        let pFieldOrMethod : Parser<PMember> =
            let pLeading = opt (pStorageClass .>>? spaces1) .>>.? pName .>>.? pContextInfo .>> spaces
    
            let pField = pTypeAnnotation .>> pKeywordSemicolon .>> spaces |>> (fun t -> Field(t))
            let pMethodSig = pMethodSignature
            let pMethodDefinition = (pKeywordSemicolon |>> (fun _ -> PMethodDeclarationOrDefinition.Definition))
            let pMethodDeclaration = pCodeBlock |>> (fun s -> PMethodDeclarationOrDefinition.Declaration s)
            let pMethodTrail = pMethodDefinition <|> pMethodDeclaration
            let pMethod = pMethodSig .>>. pMethodTrail .>> spaces |>> (fun ((x,y),z) -> PFieldOrMethod.Method(x,y,z))

            let pEither = pField <|> pMethod
            pLeading .>>. pEither |>> fun (((s, n),ctx),fm) ->
                match s with
                | Some s' -> PMember.FieldOrMethod(s', ctx, n, fm)
                | _ -> PMember.FieldOrMethod(Instance, ctx, n, fm)
        let pa = pAccessModifier [pKeywordInternal;pKeywordPrivate;pKeywordProtected;pKeywordPublic]
        let p = pa .>>. (pConstructor <|> pFieldOrMethod)
        p |>> (fun (modifier, x) ->
            match x with
            | Constructor (ctx, parameters, statements) -> Ast.Constructor(modifier,parameters,statements)
            | FieldOrMethod(storage, ctx, n,d) -> 
                match d with
                | Field t -> Ast.Field(modifier, storage, t, n)
                | Method (parameters, t, mt) ->
                    match mt with
                    | Definition -> TypeMember.MethodDefinition(modifier, storage, n, parameters, t)
                    | Declaration stmts -> TypeMember.Method (modifier, storage, n, parameters, t, stmts))
    let private pStruct : Parser<PTypeDeclaration> =
            let pBody = 
                pKeywordBraceStart >>. spaces >>. many pTypeMember .>> spaces.>> pKeywordBraceEnd .>> spaces
            pKeywordStruct >>. spaces1 >>. pName .>>. pContextInfo .>> spaces .>>. pBody 
            |>> (fun ((x, ctx), y) -> PStruct(ctx, x, y))
            <?> "struct"
    let private pClass : Parser<PTypeDeclaration> =
        let pBody = 
            pKeywordBraceStart >>. spaces >>. many pTypeMember .>> spaces.>> pKeywordBraceEnd .>> spaces
        pKeywordClass >>. spaces1 >>. pName .>>. pContextInfo .>> spaces .>>. opt (pInheritance .>> spaces) .>>. pBody 
        |>> (fun (((n,pos),t),m) -> 
            match t with
            | Some t' -> PClass(pos, n,t',m)
            | _ -> PClass(pos, n, [], m))
        <?> "struct"
    let pType =
        (pAccessModifier [pKeywordPublic;pKeywordInternal;pKeywordPrivate]) .>>. (pEnum <|> pInterface <|> pStruct <|> pClass)
        |>> fun (accessModifier, typeDefinition) ->
            match typeDefinition with
            | PEnum (ctx, name, values) -> Type.Enum(ctx, accessModifier, name, values)
            | PInterface (ctx, name, inheritance, members) -> Type.Interface(ctx, accessModifier, name, inheritance, members)
            | PStruct (ctx, name, members) -> Type.Struct(ctx, accessModifier, name, members)
            | PClass  (ctx, name, inheritance, members) -> Type.Class(ctx, accessModifier, name, inheritance, members)
    ///#endregion Types
    ///#region Compilation Unit
    let pUsingDirectives : Parser<UsingDirective list> =
        let pUsingDirective = 
            pKeywordUsing >>. spaces1 >>. pNameM .>> spaces .>> pKeywordSemicolon .>> spaces |>> (fun (Name x) -> UsingDirective (x))
        spaces >>. many pUsingDirective .>> spaces

    let pTopLevelDeclarations : Parser<TopLevelDeclaration list> =    
        let (pTopLevelDeclaration : Parser<TopLevelDeclaration>), pTopLevelDeclarationImpl = createParserForwardedToRef()
        do pTopLevelDeclarationImpl :=
            let pNamespace : Parser<Name * TopLevelDeclaration list> =
                pKeywordNamespace >>. spaces1 >>. pNameM .>> spaces .>> pKeywordBraceStart .>> spaces
                .>>. many pTopLevelDeclaration .>> spaces .>> pKeywordBraceEnd .>> spaces
            List.reduce (<|>) [pType |>> Type; pNamespace |>> Namespace]  
        spaces >>. many pTopLevelDeclaration .>> spaces

    let pCompilationUnit : Parser<CompilationUnit> =
        pUsingDirectives .>>. pTopLevelDeclarations .>> eof |>> CompilationUnit
    ///#endregion Compilation Unit