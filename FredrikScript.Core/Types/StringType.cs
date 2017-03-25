using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace FredrikScript.Core.Types
{
    public class StringType : IType
    {
        public TypeKind Kind => TypeKind.String;
        public string FullyQualifiedName => "string";
        public string Name => "string";
        public string Namespace => "";
    }
}
