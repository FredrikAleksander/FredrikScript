using System;

namespace FredrikScript.Core.Types
{
    public abstract class TypeBuilder : IType
    {
        private readonly string _namespace;
        private readonly string _name;
        private readonly string _fqn;

        public TypeBuilder(string ns, string name)
        {
            _namespace = ns ?? "";
            _name = name ?? throw new ArgumentNullException(nameof(name));
            if (string.IsNullOrWhiteSpace(ns))
                _fqn = name;
            else
                _fqn = ns + "." + name;
        }

        public string Name => _name;
        public string Namespace => _namespace;

        public string FullyQualifiedName => _fqn;

        public abstract TypeKind Kind { get; }
    }
}
