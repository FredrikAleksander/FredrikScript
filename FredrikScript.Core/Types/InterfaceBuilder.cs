using System;

namespace FredrikScript.Core.Types
{
    public class InterfaceBuilder : TypeBuilder, IType
    {
        private readonly string _namespace;
        private readonly string _name;

        public InterfaceBuilder(string ns, string name) : base(ns, name)
        {
            _name = name ?? throw new ArgumentNullException(nameof(name));
            _namespace = ns ?? "";
        }

        public override TypeKind Kind => TypeKind.Interface;
    }
}
