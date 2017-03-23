namespace FredrikScript

type IVisitor =
    abstract member VisitCompilationUnit: Ast.CompilationUnit -> unit
    abstract member VisitUsingDirective: Ast.UsingDirective -> unit
    abstract member VisitTopLevelDeclaration: Ast.TopLevelDeclaration -> unit
    abstract member VisitType: Ast.Type -> unit
    abstract member VisitTypeMember: Ast.TypeMember -> unit
    abstract member VisitInterfaceMember: Ast.InterfaceMember -> unit
    abstract member VisitEnum: Ast.InterfaceMember -> unit
    abstract member VisitStatement: Ast.Statement -> unit
    abstract member VisitExpression: Ast.Statement -> unit
