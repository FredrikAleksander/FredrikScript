using System;

namespace FredrikScript.Core.Types
{
    public class ShortType : IType
    {
        public TypeKind Kind => TypeKind.Short;

        public string FullyQualifiedName => "short";

        public string Name => "short";

        public string Namespace => "";
    }
}
