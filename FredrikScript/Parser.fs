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

    let pWhitespaceSymbolOrEof : Parser<unit> = 
        (satisfy (fun x -> 
            System.Char.IsWhiteSpace(x) || (System.Char.IsDigit(x) || System.Char.IsLetter(x)) = false
        ) >>% ()) <|> eof

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
        pName .>>. pContextInfo .>> spaces .>> pKeywordColon .>> spaces .>>. pTypeName .>> spaces 
        |>> fun ((n,c),t) -> Parameter(c,n,t)
    let pParameters : Parser<Parameter list> = sepBy pParameter (pKeywordComma .>> spaces)
    let pInvocation : Parser<ContextInfo * Expression list> = 
        let pArgs : Parser<Expression list> = sepBy pExpression (pKeywordComma .>> spaces)
        attempt (pContextInfo .>> pKeywordParensStart .>> spaces .>>. pArgs .>> pKeywordParensEnd .>> spaces)
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
        let pExtern = pKeywordExtern >>. spaces1 >>. pStringLiteral .>> spaces1 |>> (fun d -> Extern d)
        let pStatic = pKeywordStatic >>. followedBy pWhitespaceSymbolOrEof |>> (fun _ -> Static)

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
    let pCodeBlock : Parser<Statement list> =
        pKeywordBraceStart >>. spaces >>. manyTill pStatement pKeywordBraceEnd .>> spaces
    ///#endregion Helpers
    ///#region Expressions
    type private PNewObjectOrArray = PObject of Expression list | PArray of Expression
    let pNewExpression =
        let pLeading = pContextInfo .>>? pKeywordNew .>>? spaces1 .>>. pTypeName .>> spaces
        let pArray = pKeywordBracketStart >>. spaces >>. pExpression .>> pKeywordBracketEnd .>> spaces |>> PArray
        let pObject = pInvocation |>> (fun (_,r') -> PObject(r'))
        let p = pLeading .>>. (pArray <|> pObject)
        let validate (p : Parser<(ContextInfo * TypeName) * PNewObjectOrArray>) : Parser<Expression> =
            (fun stream ->
                let reply = p stream
                if(reply.Status = Error) then Reply(reply.Status, reply.Error)
                else
                    let (((ctx : ContextInfo), (typename : TypeName)), (objectOrArray : PNewObjectOrArray)) = reply.Result
                    let (TypeName (n,r)) = typename
                    match objectOrArray with
                    | PObject exprs ->
                        if (List.isEmpty r) then
                            Reply(NewObject(ctx, typename, exprs))
                        else
                            Reply(Error, messageError "Arrays cannot be constructed as objects")
                    | PArray expr ->
                        let newTypeName = TypeName(n, Array::r)
                        Reply(NewArray(ctx, newTypeName, expr)))
        validate p
    let pScopedExpression : Parser<Expression> =
        between (pstring "(" >>. spaces) (pstring ")" >>. spaces) pExpression .>> spaces
    let pBooleanExpression : Parser<Expression> =
        attempt (pContextInfo .>>. (pstring "true" <|> pstring "false") .>> followedBy pWhitespaceSymbolOrEof .>> spaces)
        |>> fun (c,s) ->
            match s with 
            | "true" -> Boolean (c, true)
            | _ -> Boolean (c, false)
    let pStringExpression : Parser<Expression> =
        attempt (pContextInfo .>>. pStringLiteral .>> followedBy pWhitespaceSymbolOrEof .>> spaces |>> String <?> "string")
    let pCharExpression : Parser<Expression> =
        attempt (pContextInfo .>>. pCharLiteral .>> followedBy pWhitespaceSymbolOrEof .>> spaces |>> Char <?> "character")
    let pNumberExpression : Parser<Expression> =
        let numberFormat =     
                NumberLiteralOptions.AllowMinusSign
                ||| NumberLiteralOptions.AllowFraction
                ||| NumberLiteralOptions.AllowExponent
        let pLongSuffix = satisfy (fun x -> x = 'l' || x = 'L') |>> fun _ -> 'L'
        let pFloatSuffix = satisfy (fun x -> x = 'f' || x = 'F') |>> fun _ -> 'F'

        let validate (p : Parser<(NumberLiteral * char option)>) : Parser<Expression> =
            fun stream ->
                let ctx = contextInfo stream.Position
                let reply = attempt p stream
                if reply.Status = Error then Reply(reply.Status, reply.Error)
                else
                    let (nl, suffix) = reply.Result
                    match suffix with
                    | Some 'F' ->
                        if nl.IsInteger then Reply(ReplyStatus.Error, messageError "Expected floating point number")
                        else Reply(Float (ctx, float32 nl.String))
                    | Some 'L' ->
                        if nl.IsInteger then Reply(Long (ctx, int64 nl.String))
                        else Reply(ReplyStatus.Error, messageError "Expected integer number")
                    | _ ->
                        if nl.IsInteger then Reply(Integer (ctx, int32 nl.String))
                        else Reply(Double (ctx, double nl.String))

        validate (numberLiteral numberFormat "number" .>>. opt (pLongSuffix <|> pFloatSuffix) .>>? followedBy pWhitespaceSymbolOrEof .>> spaces)
        <?> "number"
    let pSymbolExpression =
        let pIdent =
            let validate (p : Parser<_>) : Parser<_> = 
                let reservedKeyword = 
                    ["return";"var";"namespace";"struct";"class";
                    "interface";"enum";"using";"static";"extern";
                    "constructor";"true";"false";"void*";"void";
                    "for";"foreach";"if";"else";"break";"while";
                    "do";"goto";"continue";"in";"out";"ref";"new"]
                fun stream ->
                    let ctx = contextInfo stream.Position
                    let reply = (attempt p) stream
                    if reply.Status = Ok then
                        let res = reply.Result
                        if List.contains res reservedKeyword then Reply(ReplyStatus.Error, messageError (sprintf "Keyword '%s' is not valid as an identifier" reply.Result))
                        else Reply((ctx, reply.Result))
                    else
                        Reply((ctx, reply.Result))
    
            let pleading = satisfy (fun x -> x = '_' || (x >= 'a' && x <= 'z') || (x >= 'A' && x <= 'Z'))
            let pany     = satisfy (fun x -> x = '_' || (x >= 'a' && x <= 'z') || (x >= 'A' && x <= 'Z') || (x >= '0' && x <= '9'))
            let pname'    = many1Chars2 pleading pany
            let pname = validate pname' .>>? followedBy pWhitespaceSymbolOrEof .>> spaces
            pname |>> (fun (c,s) -> Expression.Symbol(c,s)) <?> "symbol"
        pIdent
    let chainIndexExpressions expr indices =
        let (hc,hi) = List.head indices
        let head = Index(hc, expr, hi)
        let newList = List.skip 1 indices
        let folder state (ctx', index) = 
            Index(ctx', state, index)
        List.fold folder head newList
    type private PIndexOrInvocation = PIndex of (ContextInfo * Expression) list | PInvocation of ContextInfo * Expression list
    let pLeafExpression =
        let pExtendedExpression =
            let pIndex = many1 (between pKeywordBracketStart pKeywordBracketEnd (spaces >>. pContextInfo .>>. pExpression .>> spaces)) |>> PIndex
            let pInvoke = pInvocation |>> PInvocation

            attempt (pContextInfo .>>. pSymbolExpression .>> spaces .>>. opt ((pIndex <|> pInvoke) .>> spaces))
            |>> fun ((ctx, sym), indexOrInvokeS) ->
                match indexOrInvokeS with
                | Some (PIndex indices) ->
                    chainIndexExpressions sym indices
                | Some (PInvocation (ctx,args)) ->
                    Expression.Call(ctx, sym, args)
                | _ -> sym
        let pMemberSegment =
            attempt (pContextInfo .>>. pIdentifier .>> spaces .>>. opt (between pKeywordBracketStart pKeywordBracketEnd (spaces >>. pContextInfo .>>. pExpression .>> spaces) .>> spaces) )
        let pMember = 
            let pLeading = pKeywordPeriod >>. spaces >>. pMemberSegment
            many1 pLeading .>> spaces .>>. opt (pInvocation .>> spaces)
        let pLeaf =
            let pm instance ((ctx,membr), index) =
                let m = Expression.Member(ctx, instance, membr)
                match index with
                | Some (c,i) -> Expression.Index(c, m, i)
                | _ -> m
            pContextInfo .>>. (pScopedExpression <|> pBooleanExpression <|> pStringExpression <|> pCharExpression <|> pNumberExpression <|> pNewExpression <|> pExtendedExpression) .>>. opt pMember
            |>> fun ((c,expr), (membrS)) ->
                match membrS with
                | Some (membr, invocationS) ->
                    let head =  pm expr (List.head membr)
                    let newList = List.skip 1 membr
                    let mm = List.fold pm head newList
                    match invocationS with
                    | Some (c, invocation) -> Expression.Call(c, mm, invocation)
                    | _ -> mm
                | _ -> expr
            
        pLeaf
    ///#endregion Expressions
    ///#region Operators
    type Assoc = Associativity
    let opp = new OperatorPrecedenceParser<Expression, ContextInfo, _>()       
    let pContextWs = pContextInfo .>> spaces

    opp.AddOperator(PostfixOperator("++", pContextWs, 16, true,(), fun c x -> PostfixIncrement(c,x)))
    opp.AddOperator(PostfixOperator("--", pContextWs, 16, true,(), fun c x -> PostfixDecrement(c,x)))
    opp.AddOperator(PrefixOperator("++", pContextWs, 15, true,(), fun c x -> PrefixIncrement(c,x)))
    opp.AddOperator(PrefixOperator("--", pContextWs, 15, true,(), fun c x -> PrefixDecrement(c,x)))
    opp.AddOperator(PrefixOperator("+", pContextWs, 15, true, (), fun c x -> UnaryPlus(c,x)))
    opp.AddOperator(PrefixOperator("-", pContextWs, 15, true, (), fun c x -> UnaryMinus(c,x)))
    opp.AddOperator(PrefixOperator("!", pContextWs, 15, true, (), fun c x -> Not(c,x)))
    opp.AddOperator(PrefixOperator("~", pContextWs, 15, true, (), fun c x -> BitwiseNot(c,x)))
    opp.AddOperator(InfixOperator("*", pContextWs, 13, Assoc.Left, (), fun c x y -> Multiply(c,x,y)))
    opp.AddOperator(InfixOperator("/", pContextWs, 13, Assoc.Left, (), fun c x y -> Divide(c,x,y)))
    opp.AddOperator(InfixOperator("%", pContextWs, 13, Assoc.Left, (), fun c x y -> Modulus(c,x,y)))
    opp.AddOperator(InfixOperator("+", pContextWs, 12, Assoc.Left, (), fun c x y -> Add(c,x,y)))
    opp.AddOperator(InfixOperator("-", pContextWs, 12, Assoc.Left, (), fun c x y -> Subtract(c,x,y)))
    opp.AddOperator(InfixOperator("<<", pContextWs, 11, Assoc.Left,(), fun c x y -> BitshiftLeft(c, x,y)))
    opp.AddOperator(InfixOperator(">>", pContextWs, 11, Assoc.Left,(), fun c x y -> BitshiftRight(c, x,y)))
    opp.AddOperator(InfixOperator("<", pContextWs, 10, Assoc.Left, (), fun c x y -> LowerThan(c,x,y)))
    opp.AddOperator(InfixOperator("<=", pContextWs, 10, Assoc.Left,(), fun c x y -> LowerThanEquals(c, x,y)))
    opp.AddOperator(InfixOperator(">", pContextWs, 10, Assoc.Left, (), fun c x y -> GreaterThan(c, x,y)))
    opp.AddOperator(InfixOperator(">=", pContextWs, 10, Assoc.Left,(), fun c x y -> GreaterThanEquals(c,x,y)))
    opp.AddOperator(InfixOperator("==", pContextWs, 9, Assoc.Left, (), fun c x y -> Equals(c,x, y)))
    opp.AddOperator(InfixOperator("!=", pContextWs, 9, Assoc.Left, (), fun c x y -> NotEqual(c, x,y)))
    opp.AddOperator(InfixOperator("&", pContextWs, 8, Assoc.Left,  (), fun c x y -> BitwiseAnd(c, x,y)))
    opp.AddOperator(InfixOperator("^", pContextWs, 7, Assoc.Left,  (), fun c x y -> BitwiseXor(c, x,y)))
    opp.AddOperator(InfixOperator("|", pContextWs, 6, Assoc.Left,  (), fun c x y -> BitwiseOr(c,x,y)))
    opp.AddOperator(InfixOperator("&&", pContextWs, 5, Assoc.Left, (), fun c x y -> And(c,x,y)))
    opp.AddOperator(InfixOperator("||", pContextWs, 4, Assoc.Left, (), fun c x y -> Or(c,x,y)))
    opp.AddOperator(TernaryOperator("?", pContextWs, ":", pContextWs, 3, Assoc.Right, (), fun c1 c2 x y z -> Ternary(c1, x, y, z)))
    opp.AddOperator(InfixOperator("=", pContextWs, 3, Assoc.Right, (),  fun c x y -> Assignment(c, x,y)))
    opp.AddOperator(InfixOperator("+=", pContextWs, 3, Assoc.Right,(),  fun c x y -> AddAssignment(c,x,y)))
    opp.AddOperator(InfixOperator("-=", pContextWs, 3, Assoc.Right,(),  fun c x y -> SubtractAssignment(c,x,y)))
    opp.AddOperator(InfixOperator("*=", pContextWs, 3, Assoc.Right,(),  fun c x y -> MultiplyAssignment(c,x,y)))
    opp.AddOperator(InfixOperator("/=", pContextWs, 3, Assoc.Right,(),  fun c x y -> DivideAssignment(c,x,y)))
    opp.AddOperator(InfixOperator("%=", pContextWs, 3, Assoc.Right,(),  fun c x y -> ModulusAssignment(c,x,y)))
    opp.AddOperator(InfixOperator("<<=", pContextWs, 3, Assoc.Right,(), fun c x y -> BitshiftLeftAssignment(c,x,y)))
    opp.AddOperator(InfixOperator(">>=", pContextWs, 3, Assoc.Right,(), fun c x y -> BitshiftRightAssignment(c,x,y)))
    opp.AddOperator(InfixOperator("&=", pContextWs, 3, Assoc.Right, (), fun c x y -> BitwiseAndAssignment(c,x,y)))
    opp.AddOperator(InfixOperator("^=", pContextWs, 3, Assoc.Right, (), fun c x y -> BitwiseXorAssignment(c,x,y)))
    opp.AddOperator(InfixOperator("|=", pContextWs, 3, Assoc.Right, (), fun c x y -> BitwiseOrAssignment(c,x,y)))
    opp.TermParser <- pLeafExpression
    do pExpressionImpl := opp.ExpressionParser
    ///#endregion Operators
    ///#region Statements
    let pBreakStatement =
        pKeywordBreak >>. followedBy pWhitespaceSymbolOrEof >>. pContextInfo .>> spaces |>> Break
        <?> "break statement"
    let pContinueStatement =
        pKeywordContinue >>. followedBy pWhitespaceSymbolOrEof >>. pContextInfo .>> spaces |>> Continue
        <?> "continue statement"
    let pVarStatement =
        let pVarLeading = pKeywordVar >>? spaces1 >>. pIdentifier .>>. pContextInfo .>> spaces
        let pVarTrailing = opt pTypeAnnotation .>>. opt (pKeywordEquals >>. spaces >>. pExpression .>> followedBy pWhitespaceSymbolOrEof)

        pVarLeading .>>. pVarTrailing 
        |>> (fun ((i,c), (t, d)) ->
            match d with
                | Some e -> VariableDeclaration(c, i, t, Some e)
                | _ -> VariableDeclaration(c, i, t, None))
        <?> "var statement"
    let pReturnStatement =
        attempt (pKeywordReturn >>. pContextInfo .>>. (opt (spaces1 >>? pExpression) <|> (followedBy pWhitespaceSymbolOrEof >>. spaces >>% None)))
        |>> Return <?> "return statement"
    let pWhileStatement =
        pKeywordWhile >>. pContextInfo .>> spaces .>> pKeywordParensStart .>> spaces .>>. pExpression .>> pKeywordParensEnd .>> spaces .>>. (pCodeBlock <|> (pStatement |>> fun s -> [s]))
        |>> (fun ((c, e), s) -> While(c, e, s)) <?> "while statement"
    let pDoWhileStatement =
        pKeywordDo >>. pContextInfo .>> spaces .>>. (pCodeBlock <|> (pStatement |>> fun s -> [s])) .>> pKeywordWhile .>> spaces .>> pKeywordParensStart .>> spaces .>>. pExpression .>> pKeywordParensEnd .>> spaces
        |>> (fun ((c,s),e) -> DoWhile (c,s,e)) <?> "do statement"
    let pEmptyStatement : Parser<Statement> = attempt (pContextInfo .>> pstring ";") .>> spaces |>> Empty
    let pForEachStatement =
        pKeywordForEach >>. pContextInfo .>> spaces .>> pKeywordParensStart .>> spaces .>> pKeywordVar .>> spaces1 .>>. pName .>> spaces1 .>> pKeywordIn .>> spaces1 .>>. pExpression .>>
        pKeywordParensEnd .>> spaces .>>. (pCodeBlock <|> (pStatement |>> fun s -> [s]))
        |>> (fun (((c, name), expr), statements) -> ForEach (c, name, expr, statements))
        <?> "foreach statement"
    let pExpressionStatement =
        attempt (pContextInfo .>>. pExpression |>> Statement.Expression)
        <?> "expression statement"
    let pForStatement =
        let pInitStatement = opt (pVarStatement <|> pExpressionStatement) .>> pKeywordSemicolon .>> spaces
        let pExpr = opt pExpression
        pKeywordFor >>. pContextInfo .>> spaces .>> pKeywordParensStart .>> spaces .>>. pInitStatement .>>. pExpr .>> pKeywordSemicolon .>> spaces .>>. pExpr .>> pKeywordParensEnd .>> spaces .>>. (pCodeBlock <|> (pStatement |>> fun s -> [s]))
        |>> (fun ((((ctx,initStatement), conditionStatement), afterStatement), statements) ->
            let is = match initStatement with
                     | Some is' -> is'
                     | _ -> Empty ctx
            let cs = match conditionStatement with
                     | Some cs' -> cs'
                     | _ -> Nop(ctx)
            let afs = match afterStatement with
                      | Some afs' -> afs'
                      | None -> Nop(ctx)
            For(ctx, is, cs, afs, statements))
        <?> "for statement"
    let pIfStatement =
        let pElseIfStatement = pKeywordElseIf >>. spaces >>. pContextInfo .>> pKeywordParensStart .>> spaces .>>. pExpression .>> pKeywordParensEnd .>> spaces .>>. (pCodeBlock <|> (pStatement |>> fun s -> [s])) |>> fun ((a,b),c) -> (a,b,c)
        let pElseStatement = pKeywordElse >>. spaces >>. pContextInfo .>>. (pCodeBlock <|> (pStatement |>> fun s -> [s]))
        let pIfStatement' = pKeywordIf >>. spaces >>. pContextInfo .>> pKeywordParensStart .>> spaces .>>. pExpression .>> pKeywordParensEnd .>> spaces .>>. (pCodeBlock <|> (pStatement |>> fun s -> [s]))
        pIfStatement' .>>. many pElseIfStatement .>>. opt pElseStatement |>> (fun ((((ctx, ifExpression), ifStatement), elseIfStatementsM), elseStatementS) -> If (ctx, ifExpression, ifStatement, elseIfStatementsM, elseStatementS)) <?> "if statement"
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
        let pEnumMember = pName .>>. pContextInfo .>> spaces .>>. opt pEnumValue .>> spaces |>> fun ((n, c),v) -> EnumValue(c,n,v)
        let pTrailing = pKeywordComma >>. spaces >>. pEnumMember
        let pEnumMembers = pEnumMember .>>. manyTill pTrailing pKeywordBraceEnd |>> (fun (x,y) -> x::y)
        let pMap ((x, c), y) =
            PEnum (c,x,y)
        pKeywordEnum >>. spaces1 >>. pName .>>. pContextInfo .>> spaces .>> pKeywordBraceStart .>> spaces .>>. pEnumMembers .>> spaces
        |>> pMap <?> "enum"
    let private pInterface =
        let pInterfaceMethod: Parser<InterfaceMember> =
            let p = pName .>>. pContextInfo.>> spaces .>>. pMethodSignature .>> pKeywordSemicolon .>> spaces |>> (fun ((x,c),(y,z)) ->
                Method (c, x, y, z))
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
            let pLeading = pStorageClass .>>. pName .>>. pContextInfo .>> spaces
    
            let pField = pTypeAnnotation .>> pKeywordSemicolon .>> spaces |>> (fun t -> Field(t))
            let pMethodSig = pMethodSignature
            let pMethodDefinition = (pKeywordSemicolon |>> (fun _ -> PMethodDeclarationOrDefinition.Definition))
            let pMethodDeclaration = pCodeBlock |>> (fun s -> PMethodDeclarationOrDefinition.Declaration s)
            let pMethodTrail = pMethodDefinition <|> pMethodDeclaration
            let pMethod = pMethodSig .>>. pMethodTrail .>> spaces |>> (fun ((x,y),z) -> PFieldOrMethod.Method(x,y,z))

            let pEither = pField <|> pMethod
            pLeading .>>. pEither |>> (fun (((s, n),ctx),fm) ->
                PMember.FieldOrMethod(s, ctx, n, fm)) <?> "field or method"
        let pa = pAccessModifier [pKeywordInternal;pKeywordPrivate;pKeywordProtected;pKeywordPublic]
        let p = pa .>>. (pConstructor <|> pFieldOrMethod)
        p |>> (fun (modifier, x) ->
            match x with
            | Constructor (ctx, parameters, statements) -> Ast.Constructor(ctx, modifier,parameters,statements)
            | FieldOrMethod(storage, ctx, n,d) -> 
                match d with
                | Field t -> Ast.Field(ctx, modifier, storage, t, n)
                | Method (parameters, t, mt) ->
                    match mt with
                    | Definition -> TypeMember.MethodDefinition(ctx, modifier, storage, n, parameters, t)
                    | Declaration stmts -> TypeMember.Method (ctx, modifier, storage, n, parameters, t, stmts))
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
            pKeywordUsing >>. spaces1 >>. pContextInfo .>>. pNameM .>> spaces .>> pKeywordSemicolon .>> spaces |>> (fun (c, (Name x)) -> UsingDirective (c, x))
        spaces >>. many pUsingDirective .>> spaces

    let pTopLevelDeclarations : Parser<TopLevelDeclaration list> =    
        let (pTopLevelDeclaration : Parser<TopLevelDeclaration>), pTopLevelDeclarationImpl = createParserForwardedToRef()
        do pTopLevelDeclarationImpl :=
            let pm n c s =
                 Namespace (c,n,s)
            let pNamespace : Parser<TopLevelDeclaration> =
                pKeywordNamespace >>. spaces1 >>. pNameM .>>. pContextInfo .>> spaces .>> pKeywordBraceStart .>> spaces .>>. many pTopLevelDeclaration .>> spaces .>> pKeywordBraceEnd .>> spaces
                |>> (fun ((n,c),s) -> Namespace (c,n,s))
            List.reduce (<|>) [pNamespace;(pContextInfo .>>. pType) |>> Type]  
        spaces >>. many pTopLevelDeclaration .>> spaces

    let pCompilationUnit : Parser<CompilationUnit> =
        pContextInfo .>>. pUsingDirectives .>>. pTopLevelDeclarations .>> eof |>> fun ((x,y),z) -> CompilationUnit (x,y,z)
    ///#endregion Compilation Unit


    let private parseHandler result =
        match result with
        | Success (r, _, _) -> r
        | Failure (err, _, _) -> failwith err

    let parseString str =
        let result = run pCompilationUnit str
        parseHandler result
    let parseFile file encoding =
        use fileStream = new System.IO.FileStream(file, System.IO.FileMode.Open, System.IO.FileAccess.Read, System.IO.FileShare.Read);
        let result = runParserOnStream pCompilationUnit () file fileStream encoding
        parseHandler result
    let parseStream stream encoding filename =
        let result = runParserOnStream pCompilationUnit () filename stream encoding
        parseHandler result