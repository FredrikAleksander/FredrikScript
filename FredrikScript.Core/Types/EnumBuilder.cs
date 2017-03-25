using System;

namespace FredrikScript.Core.Types
{
    public class EnumBuilder : TypeBuilder, IType
    {
        private readonly string _namespace;
        private readonly string _name;

        public EnumBuilder(string ns, string name) : base(ns, name)
        {
            _namespace = ns ?? "";
            _name = name ?? throw new ArgumentNullException(nameof(name));
        }

        public override TypeKind Kind => TypeKind.Enum;
    }
}
