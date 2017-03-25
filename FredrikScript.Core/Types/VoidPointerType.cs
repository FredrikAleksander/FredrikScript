using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace FredrikScript.Core.Types
{
    public class VoidPointerType : IType
    {
        private readonly string _namespace;
        private readonly string _name;

        public string Namespace => "void*";
        public string Name => "void*";

        public TypeKind Kind => TypeKind.Pointer;

        public string FullyQualifiedName => "void*";
    }
}
