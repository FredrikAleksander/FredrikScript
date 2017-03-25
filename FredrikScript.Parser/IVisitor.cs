using FredrikScript.ParserFS;

namespace FredrikScript.Parser
{
    public interface IVisitor
    {
        void Visit(Ast.CompilationUnit compilationUnit);
    }
}