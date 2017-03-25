using System;

namespace FredrikScript.Core.Types
{
    public class LongType : IType
    {
        public TypeKind Kind => TypeKind.Ulong;

        public string FullyQualifiedName => "long";

        public string Name => "long";

        public string Namespace => "";
    }
}
