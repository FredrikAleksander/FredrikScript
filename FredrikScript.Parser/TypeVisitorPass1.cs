using FredrikScript.Core;
using FredrikScript.ParserFS;
using System;
using System.Collections.Generic;
using System.Linq;

namespace FredrikScript.Parser
{
    /// <summary>
    /// Enumerates all namespaces and types to provide proper global symbol resolution
    /// </summary>
    public class TypeVisitorPass1 : CompilerVisitorBase
    {
        public TypeVisitorPass1(Context context) : base(context)
        {
        }

        protected override void VisitCompilationUnit(Ast.CompilationUnit compilationUnit)
        {
            base.VisitCompilationUnit(compilationUnit);
        }

        private void EnsureSymbolNamespace(string @namespace) // Throws if the current namespace conflicts with another symbol
        {
            if (_context.Types.ContainsKey(Namespace))
                throw new Exception($"Namespace conflicts with type name '{Namespace}'");
            if (!_context.Namespaces.Any(x => x == Namespace))
            {
                var sep = Namespace.LastIndexOf('.');
                if(sep >= 0)
                {
                    var parentNamespace = Namespace.Substring(0, sep);
                    EnsureSymbolNamespace(parentNamespace);
                }
                _context.EnsureNamespace(@namespace);
            }
        }

        protected override void VisitNamespace(Ast.TopLevelDeclaration.Namespace ns)
        {
            EnsureSymbolNamespace(Namespace);
            base.VisitNamespace(ns);
        }

        protected override void VisitEnum(Ast.Type.Enum eenum)
        {
            base.VisitEnum(eenum);
            _context.CreateEnum(Namespace, eenum.Item3.Item);
        }

        protected override void VisitInterface(Ast.Type.Interface iface)
        {
            _context.CreateInterface(Namespace, iface.Item3.Item);
        }

        protected override void VisitStruct(Ast.Type.Struct strr)
        {
            _context.CreateStruct(Namespace, strr.Item3.Item);
        }

        protected override void VisitClass(Ast.Type.Class clazz)
        {
            _context.CreateClass(Namespace, clazz.Item3.Item);
        }
    }
}