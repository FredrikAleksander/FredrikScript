using System;
using LLVMSharp;

namespace FredrikScript.Sandbox.TypeBuilders
{
    public abstract class TypeBuilder : IType
    {
        private readonly string _namespace;
        private readonly string _name;
        private readonly string _fqn;

        public TypeBuilder(string ns, string name)
        {
            _namespace = ns;
            _name = name;
            if (string.IsNullOrWhiteSpace(ns))
                _fqn = name;
            else
                _fqn = ns + "." + name;
        }

        public string Name => _name;
        public string Namespace => _namespace;

        public string FullyQualifiedName => _fqn;

        public abstract TypeKind Kind { get; }
        public abstract LLVMTypeRef TypeValue { get; }
    }
}
