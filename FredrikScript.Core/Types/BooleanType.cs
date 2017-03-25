using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace FredrikScript.Core.Types
{
    public class BooleanType : IType
    {
        public string FullyQualifiedName => "bool";

        public string Name => "bool";

        public string Namespace => "";

        public TypeKind Kind => TypeKind.Bool;
    }
}
