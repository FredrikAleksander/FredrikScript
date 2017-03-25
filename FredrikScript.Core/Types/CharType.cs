using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace FredrikScript.Core.Types
{
    public class CharType : IType
    {
        public string FullyQualifiedName => "char";

        public string Name => "char";

        public string Namespace => "";

        public TypeKind Kind => TypeKind.Char;
    }
}
