using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace FredrikScript.Core.Types
{
    public class ByteType : IType
    {
        public string FullyQualifiedName => "byte";

        public string Name => "byte";

        public string Namespace => "";

        public TypeKind Kind => TypeKind.Byte;
    }
}
