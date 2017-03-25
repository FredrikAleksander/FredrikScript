using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace FredrikScript.Core.Types
{
    public class IntType : IType
    {
        public TypeKind Kind => TypeKind.Int;
        public string FullyQualifiedName => "int";
        public string Name => "int";
        public string Namespace => "";
    }
}
