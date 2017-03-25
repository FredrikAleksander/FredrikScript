using System;

namespace FredrikScript.Core.Types
{
    public class DoubleType : IType
    {
        public TypeKind Kind => TypeKind.Double;

        public string FullyQualifiedName => "double";

        public string Name => "double";

        public string Namespace => "";
    }
}
