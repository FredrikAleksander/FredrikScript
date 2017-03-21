module Ast

type Name = Name of string
type TypeName = 
    TypeName of string * Rank list
and Rank = Pointer | Array

type AccessModifier = Internal|Private|Protected|Public
type StorageClass = 
    |Instance
    |Static
    |Extern of string

type CompilationUnit = CompilationUnit of UsingDirective list * TopLevelDeclaration list
and UsingDirective = UsingDirective of string
and TopLevelDeclaration =
    | Type of Type
    | Namespace of Name * TopLevelDeclaration list
and Type =
    | Enum of AccessModifier * Name * EnumValue list
    | Class of AccessModifier * Name * TypeMember list
    | Struct of AccessModifier * Name * TypeMember list
    | Interface of AccessModifier * Name * InterfaceMember list
and EnumValue = EnumValue of Name * int option
and TypeMember =
    | Constructor of AccessModifier * Parameter list * Statement list
    | Field of AccessModifier * StorageClass * TypeName * Name
    | Method of AccessModifier * StorageClass * TypeName * Name * Parameter list * Statement list
    | MethodDefinition of AccessModifier * StorageClass * TypeName * Name * Parameter list
and InterfaceMember =
    | Method of TypeName * Name * Parameter list
and Parameter =
    | Parameter of Name * TypeName
and Statement =
    | Empty
    | Expression of Expression
    | VariableDeclaration of string * TypeName option * Expression option
    | For of Statement * Expression * Expression * Statement list
    | ForEach of Name * Expression * Statement list
    | If of Expression * Statement list * (Expression * Statement list) list * Statement list option
    | While of Expression * Statement list
    | DoWhile of Statement list * Expression
    | Break
    | Continue
    | Return of Expression option
and Expression =
    | Integer of int32
    | Long of int64
    | Float of float32
    | Double of double
    | Boolean of bool
    | Char of char
    | String of string
    | Add of Expression * Expression
    | Subtract of Expression * Expression
    | Multiply of Expression * Expression
    | Divide of Expression * Expression
    | Modulus of Expression * Expression
    | BitshiftLeft of Expression * Expression
    | BitshiftRight of Expression * Expression
    | Ternary of Expression * Expression * Expression
    | UnaryPlus of Expression
    | UnaryMinus of Expression
    | Not of Expression
    | BitwiseNot of Expression
    | PrefixIncrement of Expression
    | PrefixDecrement of Expression
    | SuffixIncrement of Expression
    | PostfixIncrement of Expression
    | PostfixDecrement of Expression
    | And of Expression * Expression
    | Or of Expression * Expression
    | BitwiseAnd of Expression * Expression
    | BitwiseOr of Expression * Expression
    | BitwiseXor of Expression * Expression
    | Equals of Expression * Expression
    | NotEqual of Expression * Expression
    | GreaterThan of Expression * Expression
    | GreaterThanEquals of Expression * Expression
    | LowerThan of Expression * Expression
    | LowerThanEquals of Expression * Expression
    | Assignment of Expression * Expression
    | AddAssignment of Expression * Expression
    | SubtractAssignment of Expression * Expression
    | MultiplyAssignment of Expression * Expression
    | DivideAssignment of Expression * Expression
    | ModulusAssignment of Expression * Expression
    | BitshiftLeftAssignment of Expression * Expression
    | BitshiftRightAssignment of Expression * Expression
    | BitwiseAndAssignment of Expression * Expression
    | BitwiseXorAssignment of Expression * Expression
    | BitwiseOrAssignment of Expression * Expression
    | NewArray of TypeName * Expression
    | NewObject of TypeName * Expression list
    | Symbol of string
    | Member of Expression * string
    | Index of Expression * Expression
    | Call of Expression * Expression list
    | Nop
