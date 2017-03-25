using FredrikScript.Core;
using FredrikScript.Core.Types;
using FredrikScript.ParserFS;
using System;
using System.Text;

namespace FredrikScript.Parser
{
    public class CompilerVisitorBase : VisitorBase
    {
        protected readonly Context _context;

        public CompilerVisitorBase(Context context)
        {
            _context = context ?? throw new ArgumentNullException(nameof(context));
        }

        public IType ResolveType(string typeName)
        {
            return _context.ResolveType(typeName, Namespace, UsingDirectives);
        }

        public IType ResolveType(Ast.TypeName typeNameAst)
        {
            StringBuilder str = new StringBuilder();
            str.Append(typeNameAst.Item1);
            foreach (var rank in typeNameAst.Item2)
                str.Append(rank.IsPointer ? "*" : "[]");
            return _context.ResolveType(str.ToString(), Namespace, UsingDirectives);
        }
    }
}