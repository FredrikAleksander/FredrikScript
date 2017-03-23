namespace FredrikScript

module Ast =
    type ContextInfo(streamName : string, index : int64, line : int64, column : int64) = 
        let streamName' = 
            match (System.String.IsNullOrWhiteSpace streamName) with
            | true -> "<Unknown>"
            | _ -> streamName

        member this.StreamName = streamName'
        member this.Index = index
        member this.Line = line
        member this.Column = column

        override this.ToString () =
            sprintf "%s: Line %i, Column %i" this.StreamName this.Line this.Column

    type Name = Name of string
    type TypeName = 
        TypeName of string * Rank list
    and Rank = Pointer | Array

    type AccessModifier = Internal|Private|Protected|Public
    type StorageClass = 
        |Instance
        |Static
        |Extern of string

    type CompilationUnit = CompilationUnit of ContextInfo * UsingDirective list * TopLevelDeclaration list
    and UsingDirective = UsingDirective of ContextInfo * string
    and TopLevelDeclaration =
        | Type of ContextInfo * Type
        | Namespace of ContextInfo * Name * TopLevelDeclaration list
    and Type =
        | Enum of ContextInfo * AccessModifier * Name * EnumValue list
        | Class of ContextInfo * AccessModifier * Name * TypeName list * TypeMember list
        | Struct of ContextInfo * AccessModifier * Name * TypeMember list
        | Interface of ContextInfo * AccessModifier * Name * TypeName list * InterfaceMember list
    and EnumValue = EnumValue of ContextInfo * Name * int option
    and TypeMember =
        | Constructor of ContextInfo * AccessModifier * Parameter list * Statement list
        | Field of ContextInfo * AccessModifier * StorageClass * TypeName * Name
        | Method of ContextInfo * AccessModifier * StorageClass * Name * Parameter list * TypeName * Statement list
        | MethodDefinition of ContextInfo * AccessModifier * StorageClass * Name * Parameter list * TypeName
    and InterfaceMember =
        | Method of ContextInfo * Name * Parameter list * TypeName
    and Parameter =
        | Parameter of ContextInfo * Name * TypeName
    and Statement =
        | Empty of ContextInfo
        | Expression of ContextInfo * Expression
        | VariableDeclaration of ContextInfo * string * TypeName option * Expression option
        | For of ContextInfo * Statement * Expression * Expression * Statement list
        | ForEach of ContextInfo * Name * Expression * Statement list
        | If of ContextInfo * Expression * Statement list * (ContextInfo * Expression * Statement list) list * (ContextInfo * Statement list) option
        | While of ContextInfo * Expression * Statement list
        | DoWhile of ContextInfo * Statement list * Expression
        | Break of ContextInfo
        | Continue of ContextInfo
        | Return of ContextInfo * Expression option
    and Expression =
        | Integer of ContextInfo * int32
        | Long of ContextInfo * int64
        | Float of ContextInfo * float32
        | Double of ContextInfo * double
        | Boolean of ContextInfo * bool
        | Char of ContextInfo * char
        | String of ContextInfo * string
        | Add of ContextInfo * Expression * Expression
        | Subtract of ContextInfo * Expression * Expression
        | Multiply of ContextInfo * Expression * Expression
        | Divide of ContextInfo * Expression * Expression
        | Modulus of ContextInfo * Expression * Expression
        | BitshiftLeft of ContextInfo * Expression * Expression
        | BitshiftRight of ContextInfo * Expression * Expression
        | Ternary of ContextInfo * Expression * Expression * Expression
        | UnaryPlus of ContextInfo * Expression
        | UnaryMinus of ContextInfo * Expression
        | Not of ContextInfo * Expression
        | BitwiseNot of ContextInfo * Expression
        | PrefixIncrement of ContextInfo * Expression
        | PrefixDecrement of ContextInfo * Expression
        | SuffixIncrement of ContextInfo * Expression
        | PostfixIncrement of ContextInfo * Expression
        | PostfixDecrement of ContextInfo * Expression
        | And of ContextInfo * Expression * Expression
        | Or of ContextInfo * Expression * Expression
        | BitwiseAnd of ContextInfo * Expression * Expression
        | BitwiseOr of ContextInfo * Expression * Expression
        | BitwiseXor of ContextInfo * Expression * Expression
        | Equals of ContextInfo * Expression * Expression
        | NotEqual of ContextInfo * Expression * Expression
        | GreaterThan of ContextInfo * Expression * Expression
        | GreaterThanEquals of ContextInfo * Expression * Expression
        | LowerThan of ContextInfo * Expression * Expression
        | LowerThanEquals of ContextInfo * Expression * Expression
        | Assignment of ContextInfo * Expression * Expression
        | AddAssignment of ContextInfo * Expression * Expression
        | SubtractAssignment of ContextInfo * Expression * Expression
        | MultiplyAssignment of ContextInfo * Expression * Expression
        | DivideAssignment of ContextInfo * Expression * Expression
        | ModulusAssignment of ContextInfo * Expression * Expression
        | BitshiftLeftAssignment of ContextInfo * Expression * Expression
        | BitshiftRightAssignment of ContextInfo * Expression * Expression
        | BitwiseAndAssignment of ContextInfo * Expression * Expression
        | BitwiseXorAssignment of ContextInfo * Expression * Expression
        | BitwiseOrAssignment of ContextInfo * Expression * Expression
        | NewArray of ContextInfo * TypeName * Expression
        | NewObject of ContextInfo * TypeName * Expression list
        | Symbol of ContextInfo * string
        | Member of ContextInfo * Expression * string
        | Index of ContextInfo * Expression * Expression
        | Call of ContextInfo * Expression * Expression list
        | Nop of ContextInfo

    let printAst (ast : CompilationUnit) =
        printf "%A" ast