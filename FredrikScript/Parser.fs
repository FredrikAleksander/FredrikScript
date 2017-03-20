module Parser

open Ast
open FParsec

type UserState = unit
type Assoc = Associativity
type Parser<'a> = Parser<'a, UserState>

let pWhitespace : Parser<unit> = spaces
let pWhitespace1 : Parser<unit> = spaces1
let pStrWhitespace s = pstring s >>. pWhitespace

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
let pKeywordBrackets : Parser<string> = pchar '[' >>. pWhitespace >>. pchar ']' >>% "[]"
let pKeywordVar : Parser<string> = pstring "var"

let pCharLiteral : Parser<char> =
    let normalChar = satisfy (fun c -> c <> '\\' && c <> '\'' && c <> '\n' && c <> '\r' && c <> '\t')
    let unescape c = match c with
                     | 'n' -> '\n'
                     | 'r' -> '\r'
                     | 't' -> '\t'
                     | c   -> c
    let escapedChar = pstring "\\" >>. (anyOf "\\nrt'" |>> unescape)
    between (pstring "\"") (pstring "\"") (normalChar <|> escapedChar)
let pStringLiteral : Parser<string> =
    let normalChar = satisfy (fun c -> c <> '\\' && c <> '"')
    let unescape c = match c with
                     | 'n' -> '\n'
                     | 'r' -> '\r'
                     | 't' -> '\t'
                     | c   -> c
    let escapedChar = pstring "\\" >>. (anyOf "\\nrt\"" |>> unescape)
    between (pstring "\"") (pstring "\"")
            (manyChars (normalChar <|> escapedChar))
let pIdentifier : Parser<string> =
    let pleading = satisfy (fun x -> x = '_' || (x >= 'a' && x <= 'z') || (x >= 'A' && x <= 'Z'))
    let pany     = satisfy (fun x -> x = '_' || (x >= 'a' && x <= 'z') || (x >= 'A' && x <= 'Z') || (x >= '0' && x <= '9'))
    let pname    = many1Chars2 pleading pany

    let validate (p : Parser<string>) : Parser<string> =
        let reservedKeyword = ["return";"var"]

        fun stream ->
            let reply = p stream
            if reply.Status = Ok then
                if List.contains reply.Result reservedKeyword then Reply(ReplyStatus.Error, messageError (sprintf "Keyword is invalid as identifier '%s'" reply.Result))
                else reply
            else
                reply
    validate pname <?> "Name"

let pIdentifierM : Parser<string> =
    let ptrail   = pKeywordPeriod >>. pIdentifier

    let p = pIdentifier .>>. many ptrail |>> (fun (x,y) -> 
        let r = y |> List.fold (fun i j ->
                i + "." + j
                ) x
        r)
    p <?> "Name"

let pName : Parser<Name> =
    pIdentifier |>> Name <?> "Name"
let pNameM : Parser<Name> =
    pIdentifierM |>> Name <?> "Name"
let pTypeName : Parser<TypeName> =
    let pVoid = pKeywordVoid >>. opt (pKeywordStar)
                |>> fun s ->
                    match s with
                    | Some _ -> TypeName ("void", [Pointer])
                    | _ -> TypeName ("void", [])
    let pOther = pIdentifierM .>>. (opt (many (pKeywordBrackets |>> fun _ -> Rank.Array))) |>> fun (i,r) ->
        match r with
        | Some ra -> TypeName(i, ra)
        | _ -> TypeName(i, [])
    pVoid <|> pOther <?> "TypeName"

type private PIndexOrCall = Call of Expression list | Index of Expression
type private PIndexOrMember = Index of Expression * (MemberSegment list * Expression list option) option | Member of (MemberSegment list * Expression list option)
type private PVariableDefinitionOrDeclaration = Declaration | Definition of Expression

let pCodeBlock : Parser<Statement list> =
    let ((pExpression : Parser<Expression>), pExpressionImpl) = createParserForwardedToRef()
    
    let pScopedExpr : Parser<Expression> =
        between (pStrWhitespace "(") (pStrWhitespace ")") pExpression .>> pWhitespace


    do pExpressionImpl :=
        let pNumberExpression : Parser<Expression> =
            let numberFormat =     
                   NumberLiteralOptions.AllowMinusSign
                   ||| NumberLiteralOptions.AllowFraction
                   ||| NumberLiteralOptions.AllowExponent
            let pLongSuffix = satisfy (fun x -> x = 'l' || x = 'L') |>> fun _ -> 'L'
            let pFloatSuffix = satisfy (fun x -> x = 'f' || x = 'F') |>> fun _ -> 'F'

            numberLiteral numberFormat "Number" .>>. opt (pLongSuffix <|> pFloatSuffix) .>> pWhitespace
            |>> (fun (nl, suffix) ->
                match suffix with
                | Some 'F' ->
                    if nl.IsInteger then failwith "Expected floating point number"
                    else Float (float32 nl.String)
                | Some 'L' ->
                    if nl.IsInteger then Long (int64 nl.String)
                    else failwith "Expected integer number"
                | _ ->
                    if nl.IsInteger then Integer (int32 nl.String)
                    else Double (double nl.String))
            <?> "Number Literal"
        let pBooleanExpression : Parser<Expression> =
            let pTrue = pstring "true" |>> (fun _ -> Boolean true)
            let pFalse = pstring "false" |>> (fun _ -> Boolean false)
            (pTrue <|> pFalse) .>> pWhitespace <?> "Boolean"
        let pStringExpression : Parser<Expression> =
            pStringLiteral .>> pWhitespace |>> String <?> "String Literal"
        let pCharExpression : Parser<Expression> =
            pCharLiteral .>> pWhitespace |>> Char <?> "Character Literal"
        let pMemberSegment =
            pIdentifier .>> pWhitespace .>>. opt (between pKeywordBracketStart pKeywordBracketEnd pExpression .>> pWhitespace) 
            |>> fun (s,e) -> if (s = "return" || s = "var") then failwith (sprintf "Unsupported symbol '%s'" s) else MemberSegment(s, e)
        let pInvocation : Parser<Expression list> = 
            let pArgs : Parser<Expression list> = Primitives.sepBy pExpression (pKeywordComma .>> pWhitespace)
            pKeywordParensStart >>. pWhitespace >>. pArgs .>> pKeywordParensEnd .>> pWhitespace
        let pVariableOrCallExpression : Parser<Expression> =
            let pCall : Parser<Expression list> = pInvocation

            pMemberSegment .>>. opt pCall
            |>> (fun (identifier, vc) ->
                match vc with
                | Some args -> Expression.Call (Symbol(identifier), args)
                | _ ->  Expression.Symbol (identifier))
        
        let leaf : Parser<Expression> =
            pNumberExpression <|> pStringExpression <|> pCharExpression <|> pBooleanExpression <|> pVariableOrCallExpression  <|> pScopedExpr
        
        let pMember = 
            let pLeading = pKeywordPeriod >>. pMemberSegment
            many1 pLeading
        let term : Parser<Expression> =
            let pi = pKeywordBracketStart >>. pWhitespace >>. pExpression .>> pKeywordBracketEnd .>> pWhitespace
            let px = pMember .>>. opt pInvocation
            let py = pi .>>. opt px |>> Index
            let pxx = px |>> Member

            leaf .>>. opt (pxx <|> py)
            |>> fun (e,m) -> 
                match m with
                | Some im ->
                    match im with
                    | Index (ex, m) ->
                        let idx = Expression.Index (e, ex)
                        match m with
                        | Some (ml, el) ->
                            let mi = Expression.Member(idx, List.head ml)
                            let newList = List.skip 1 ml
                            let mr a b =
                                Expression.Member (a, b)
                            let mm = List.fold mr mi newList 
                            match el with
                            | Some els ->
                                Expression.Call(mm, els)
                            | _ -> mm
                        | _ ->
                            idx

                    | Member (ml, el) ->
                        let mi = Expression.Member(e, List.head ml)
                        let newList = List.skip 1 ml
                        let mr a b =
                            Expression.Member (a, b)
                        let mm = List.fold mr mi newList 

                        match el with
                            | Some els -> 
                                Expression.Call(mm, els)
                            | _ ->
                                mm
                | _ ->
                    e

        let opp = new OperatorPrecedenceParser<Expression, _, _>()
        opp.TermParser <- term
        
        opp.AddOperator(PostfixOperator("++", pWhitespace, 16, true, fun x -> PostfixIncrement(x)))
        opp.AddOperator(PostfixOperator("--", pWhitespace, 16, true, fun x -> PostfixDecrement(x)))
        opp.AddOperator(PrefixOperator("++", pWhitespace, 15, true, fun x -> PrefixIncrement(x)))
        opp.AddOperator(PrefixOperator("--", pWhitespace, 15, true, fun x -> PrefixDecrement(x)))
        opp.AddOperator(PrefixOperator("+", pWhitespace, 15, true, fun x -> UnaryPlus(x)))
        opp.AddOperator(PrefixOperator("-", pWhitespace, 15, true, fun x -> UnaryMinus(x)))
        opp.AddOperator(PrefixOperator("!", pWhitespace, 15, true, fun x -> Not(x)))
        opp.AddOperator(PrefixOperator("~", pWhitespace, 15, true, fun x -> BitwiseNot(x)))
        //opp.AddOperator(InfixOperator(".", pWhitespace, 14, Assoc.Left, fun x y -> Member(x,y)))
        opp.AddOperator(InfixOperator("*", pWhitespace, 13, Assoc.Left, fun x y -> Multiply(x,y)))
        opp.AddOperator(InfixOperator("/", pWhitespace, 13, Assoc.Left, fun x y -> Divide(x, y)))
        opp.AddOperator(InfixOperator("%", pWhitespace, 13, Assoc.Left, fun x y -> Modulus(x,y)))
        opp.AddOperator(InfixOperator("+", pWhitespace, 12, Assoc.Left, fun x y -> Add(x,y)))
        opp.AddOperator(InfixOperator("-", pWhitespace, 12, Assoc.Left, fun x y -> Subtract(x,y)))
        opp.AddOperator(InfixOperator("<<", pWhitespace, 11, Assoc.Left, fun x y -> BitshiftLeft(x,y)))
        opp.AddOperator(InfixOperator(">>", pWhitespace, 11, Assoc.Left, fun x y -> BitshiftRight(x,y)))
        opp.AddOperator(InfixOperator("<", pWhitespace, 10, Assoc.Left, fun x y -> LowerThan(x,y)))
        opp.AddOperator(InfixOperator("<=", pWhitespace, 10, Assoc.Left, fun x y -> LowerThanEquals(x,y)))
        opp.AddOperator(InfixOperator(">", pWhitespace, 10, Assoc.Left, fun x y -> GreaterThan(x,y)))
        opp.AddOperator(InfixOperator(">=", pWhitespace, 10, Assoc.Left, fun x y -> GreaterThanEquals(x,y)))
        opp.AddOperator(InfixOperator("==", pWhitespace, 9, Assoc.Left, fun x y -> Equals(x, y)))
        opp.AddOperator(InfixOperator("!=", pWhitespace, 9, Assoc.Left, fun x y -> NotEqual(x,y)))
        opp.AddOperator(InfixOperator("&", pWhitespace, 8, Assoc.Left, fun x y -> BitwiseAnd(x,y)))
        opp.AddOperator(InfixOperator("^", pWhitespace, 7, Assoc.Left, fun x y -> BitwiseXor(x,y)))
        opp.AddOperator(InfixOperator("|", pWhitespace, 6, Assoc.Left, fun x y -> BitwiseOr(x,y)))
        opp.AddOperator(InfixOperator("&&", pWhitespace, 5, Assoc.Left, fun x y -> And(x,y)))
        opp.AddOperator(InfixOperator("||", pWhitespace, 4, Assoc.Left, fun x y -> Or(x,y)))
        opp.AddOperator(TernaryOperator("?", pWhitespace, ":", pWhitespace, 3, Assoc.Right, fun x y z -> Ternary(x, y, z)))
        opp.AddOperator(InfixOperator("=", pWhitespace, 3, Assoc.Right, fun x y -> Assignment(x,y)))
        opp.AddOperator(InfixOperator("+=", pWhitespace, 3, Assoc.Right, fun x y -> AddAssignment(x,y)))
        opp.AddOperator(InfixOperator("-=", pWhitespace, 3, Assoc.Right, fun x y -> SubtractAssignment(x,y)))
        opp.AddOperator(InfixOperator("*=", pWhitespace, 3, Assoc.Right, fun x y -> MultiplyAssignment(x,y)))
        opp.AddOperator(InfixOperator("/=", pWhitespace, 3, Assoc.Right, fun x y -> DivideAssignment(x,y)))
        opp.AddOperator(InfixOperator("%=", pWhitespace, 3, Assoc.Right, fun x y -> ModulusAssignment(x,y)))
        opp.AddOperator(InfixOperator("<<=", pWhitespace, 3, Assoc.Right, fun x y -> BitshiftLeftAssignment(x,y)))
        opp.AddOperator(InfixOperator(">>=", pWhitespace, 3, Assoc.Right, fun x y -> BitshiftRightAssignment(x,y)))
        opp.AddOperator(InfixOperator("&=", pWhitespace, 3, Assoc.Right, fun x y -> BitwiseAndAssignment(x,y)))
        opp.AddOperator(InfixOperator("^=", pWhitespace, 3, Assoc.Right, fun x y -> BitwiseXorAssignment(x,y)))
        opp.AddOperator(InfixOperator("|=", pWhitespace, 3, Assoc.Right, fun x y -> BitwiseOrAssignment(x,y)))
        opp.ExpressionParser
    let pStatement =
        let pVarStatement =
            let pTypeAnnotation = pKeywordColon >>. pWhitespace >>. pTypeName .>> pWhitespace
            let pVarLeading = pKeywordVar >>? pWhitespace1 >>. pIdentifier .>> pWhitespace
            let pVarTrailing = opt pTypeAnnotation .>>. ((pKeywordSemicolon >>. pWhitespace |>> fun _ -> PVariableDefinitionOrDeclaration.Declaration) <|> (pKeywordEquals >>. pWhitespace >>. pExpression .>> pWhitespace .>> pKeywordSemicolon .>> pWhitespace |>> PVariableDefinitionOrDeclaration.Definition))

            pVarLeading .>>. pVarTrailing 
            |>> fun (i, (t, d)) ->
                match d with
                    | Declaration -> VariableDeclaration(i, t, None)
                    | Definition e -> VariableDeclaration(i, t, Some e)

        let pReturnStatement =
            let pReturnLeading =
                pKeywordReturn >>? pWhitespace1
            let pReturnEmptyNoWs = pstring "return;" .>> pWhitespace |>> fun _ -> Return None
            let pReturnExpr = 
                pExpression .>> pKeywordSemicolon .>> pWhitespace |>> (fun e -> Return (Some e))
            let pReturnEmpty =
                pKeywordSemicolon .>> pWhitespace >>% (Return None)
            let pReturnM = pReturnLeading >>. (pReturnEmpty <|> pReturnExpr)

            pReturnEmptyNoWs <|> pReturnM
        let pExprStatement =
                pExpression .>> pKeywordSemicolon .>> pWhitespace |>> Statement.Expression
        pReturnStatement <|> pVarStatement <|> pExprStatement
    pKeywordBraceStart >>. pWhitespace >>. manyTill pStatement pKeywordBraceEnd .>> pWhitespace

let pParameter : Parser<Parameter> = pTypeName .>> pWhitespace1 .>>. pName .>> pWhitespace |>> (fun x -> Parameter x)
let pParameters : Parser<Parameter list> = sepBy pParameter (pKeywordComma .>> pWhitespace)

let pMethodSignature : Parser<Parameter list> =
    let pMethodParametersStart = pKeywordParensStart >>. pWhitespace
    
    let pMethodParametersEnd = pKeywordParensEnd >>. pWhitespace
    pMethodParametersStart >>. pParameters .>> pMethodParametersEnd

let pStorageClass : Parser<StorageClass> =
    let pExtern = pKeywordExtern >>. pWhitespace1 >>. pStringLiteral .>> pWhitespace |>> (fun d -> Extern d)
    let pStatic = pKeywordStatic >>. pWhitespace1 |>> (fun _ -> Static)

    let p = opt (pStatic <|> pExtern)
        |>> function
        | Some d -> d
        | _ -> Instance
    p <?> "Storage Class"

let pAccessModifier a : Parser<AccessModifier> =
    let p =
        List.reduce (<|>) a |>> function
                    | "public" -> Public
                    | "protected" -> Protected
                    | "private" -> Private
                    | "internal" -> Internal
                    | _ -> failwith ""
    opt (p .>> pWhitespace1) .>> pWhitespace |>> (fun x ->
        match x with
        | Some r -> r
        | _ -> Private) <?> "Access Modifier"
    

type private PMethodDeclarationOrDefinition =
    | Declaration of Statement list
    | Definition
type private PFieldOrMethod =
    | Field
    | Method of Parameter list * PMethodDeclarationOrDefinition
type private PMember =
    | FieldOrMethod of StorageClass * TypeName * Name * PFieldOrMethod
    | Constructor of Parameter list * Statement list

let pMember : Parser<TypeMember> =
    let pConstructor : Parser<PMember> =
        pKeywordConstructor >>. pWhitespace >>. pKeywordParensStart >>. pWhitespace >>. pParameters .>> pKeywordParensEnd .>> pWhitespace .>>. pCodeBlock .>> pWhitespace |>> Constructor <?> "Constructor"

    let pFieldOrMethod : Parser<PMember> =
        let pLeading = pStorageClass .>>. pTypeName .>> pWhitespace1 .>>. pName .>> pWhitespace
    
        let pField = pKeywordSemicolon .>> pWhitespace |>> (fun _ -> Field)
        let pMethodSig = pMethodSignature
        let pMethodDefinition = (pKeywordSemicolon |>> (fun _ -> PMethodDeclarationOrDefinition.Definition))
        let pMethodDeclaration = pCodeBlock |>> (fun s -> PMethodDeclarationOrDefinition.Declaration s)
        let pMethodTrail = pMethodDefinition <|> pMethodDeclaration
        let pMethod = pMethodSig .>>. pMethodTrail .>> pWhitespace |>> (fun x -> PFieldOrMethod.Method(x))

        let pEither = pField <|> pMethod
        pLeading .>>. pEither |>> (fun (((s,t), n), o) -> PMember.FieldOrMethod (s, t, n, o))

    let pa = pAccessModifier [pKeywordInternal;pKeywordPrivate;pKeywordProtected;pKeywordPublic]
    let p = pa .>>. (pConstructor <|> pFieldOrMethod)
    p |>> (fun (modifier, x) ->
        match x with
        | Constructor (parameters, statements) -> Ast.Constructor(modifier,parameters,statements)
        | FieldOrMethod(storage,t,n,d) -> 
            match d with
            | Field _ -> Ast.Field(modifier, storage, t, n)
            | Method (parameters, mt) ->
                match mt with
                | Definition -> TypeMember.MethodDefinition(modifier, storage, t, n, parameters)
                | Declaration stmts -> TypeMember.Method (modifier, storage, t, n, parameters, stmts)
                
    )
    
type private PType = 
    | Enum of Name * EnumValue list
    | Class of Name * TypeMember list
    | Struct of Name * TypeMember list
    | Interface of Name * InterfaceMember list

let pType : Parser<Type> =
    let pEnum : Parser<PType> =
        let pa = pAccessModifier [pKeywordInternal;pKeywordPrivate;pKeywordPublic]
        let pEnumValue = pKeywordEquals >>. pWhitespace >>. pint32
        let pEnumMember = pName .>>  pWhitespace .>>. opt pEnumValue .>> pWhitespace |>> EnumValue
        let pTrailing = pKeywordComma >>. pWhitespace >>. pEnumMember
        let pEnumMembers = pEnumMember .>>. manyTill pTrailing pKeywordBraceEnd |>> (fun (x,y) -> x::y)
        let pMap (x, y) =
            Enum (x,y)

        pKeywordEnum >>. pWhitespace1 >>. pName .>> pWhitespace .>> pKeywordBraceStart .>> pWhitespace .>>. pEnumMembers .>> pWhitespace
        |>> pMap <?> "Enum"
    let pInterface : Parser<PType> =
        let pInterfaceMethod: Parser<InterfaceMember> =
            let p = pTypeName .>> pWhitespace1 .>>. pName .>> pWhitespace .>> pKeywordParensStart .>> pWhitespace .>>. pParameters .>> pWhitespace .>> pKeywordParensEnd .>> pWhitespace .>> pKeywordSemicolon .>> pWhitespace |>> (fun ((x,y),z) ->
                InterfaceMember.Method (x, y, z))
            p <?> "Method"
        let pMethods : Parser<InterfaceMember list> = 
            manyTill pInterfaceMethod pKeywordBraceEnd
        let pMap (x, y) =
            PType.Interface(x, y)
    
        pKeywordInterface >>. pWhitespace1 >>. pName .>> pWhitespace .>> pKeywordBraceStart .>> pWhitespace .>>. pMethods .>> pWhitespace
        |>> pMap
        <?> "Interface"
    let pStruct : Parser<PType> =
        let pBody = 
            pKeywordBraceStart >>. pWhitespace >>. many pMember .>> pWhitespace .>> pKeywordBraceEnd .>> pWhitespace
        pKeywordStruct >>. pWhitespace1 >>. pName .>> pWhitespace .>>. pBody |>> Struct
        <?> "Struct"
    let pClass : Parser<PType> =
        let pBody = 
            pKeywordBraceStart >>. pWhitespace >>. many pMember .>> pWhitespace .>> pKeywordBraceEnd .>> pWhitespace
        pKeywordClass >>. pWhitespace1 >>. pName .>> pWhitespace .>>. pBody |>> Class
        <?> "Class"
    let p = pClass <|> pStruct <|> pEnum <|> pInterface
    pAccessModifier [pKeywordPublic;pKeywordPrivate;pKeywordInternal] .>>. p |>> 
        (fun (x,y) ->
            match y with
            | Enum (n,v) -> Type.Enum (x, n,v)
            | Interface (n,v) -> Type.Interface (x, n, v)
            | Struct (n,v) -> Type.Struct (x, n, v)
            | Class (n, v) -> Type.Class (x, n, v)
        )

let pUsingDirectives : Parser<UsingDirective list> =
    let pUsingDirective = 
        pKeywordUsing >>. pWhitespace1 >>. pNameM .>> pWhitespace .>> pKeywordSemicolon .>> pWhitespace |>> (fun (Name x) -> UsingDirective (x))
    pWhitespace >>. many pUsingDirective .>> pWhitespace

let pTopLevelDeclarations : Parser<TopLevelDeclaration list> =    
    let (pTopLevelDeclaration : Parser<TopLevelDeclaration>), pTopLevelDeclarationImpl = createParserForwardedToRef()
    do pTopLevelDeclarationImpl :=
        let pNamespace : Parser<Name * TopLevelDeclaration list> =
            pKeywordNamespace >>. pWhitespace1 >>. pNameM .>> pWhitespace .>> pKeywordBraceStart .>> pWhitespace
            .>>. many pTopLevelDeclaration .>> pWhitespace .>> pKeywordBraceEnd .>> pWhitespace
        List.reduce (<|>) [pType |>> Type; pNamespace |>> Namespace]
        
    pWhitespace >>. many pTopLevelDeclaration .>> pWhitespace

let pCompilationUnit : Parser<CompilationUnit> =
    pUsingDirectives .>>. pTopLevelDeclarations .>> eof |>> CompilationUnit