using System;

namespace FredrikScript.Core.Types
{
    public class VoidType : IType
    {
        private readonly string _namespace;
        private readonly string _name;

        public string Namespace => "void";
        public string Name => "void";

        public TypeKind Kind => TypeKind.Void;

        public string FullyQualifiedName => "void";
    }
}
