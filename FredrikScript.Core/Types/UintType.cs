using System;

namespace FredrikScript.Core.Types
{
    public class UintType : IType
    {
        public TypeKind Kind => TypeKind.Uint;
        public string FullyQualifiedName => "uint";
        public string Name => "uint";
        public string Namespace => "";
    }
}
