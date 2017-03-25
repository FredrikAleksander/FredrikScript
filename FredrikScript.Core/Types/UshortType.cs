using System;

namespace FredrikScript.Core.Types
{
    public class UshortType : IType
    {
        public TypeKind Kind => TypeKind.Ushort;

        public string FullyQualifiedName => "ushort";

        public string Name => "ushort";

        public string Namespace => "";
    }
}
