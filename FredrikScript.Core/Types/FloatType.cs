using System;

namespace FredrikScript.Core.Types
{
    public class FloatType : IType
    {
        public TypeKind Kind => TypeKind.Float;
        public string FullyQualifiedName => "float";
        public string Name => "float";
        public string Namespace => "";
    }
}
