using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace FredrikScript.Core.Types
{
    public class SbyteType : IType
    {
        public string FullyQualifiedName => "sbyte";

        public string Name => "sbyte";

        public string Namespace => "";

        public TypeKind Kind => TypeKind.Sbyte;
    }
}
