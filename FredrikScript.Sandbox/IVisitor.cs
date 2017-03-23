namespace FredrikScript.Sandbox
{
    public interface IVisitor
    {
        void VisitCompilationUnit(Ast.CompilationUnit compilationUnit);
    }
}