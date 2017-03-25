using System;

namespace FredrikScript.Core.Types
{
    public class UlongType : IType
    {
        public TypeKind Kind => TypeKind.Ulong;

        public string FullyQualifiedName => "ulong";

        public string Name => "ulong";

        public string Namespace => "";
    }
}
