using LLVMSharp;
using System;

namespace FredrikScript.Sandbox.TypeBuilders
{
    public class StructBuilder : TypeBuilder
    {
        private readonly string _namespace;
        private readonly string _name;
        private readonly CompilerContext _context;
        private readonly LLVMTypeRef _typeValue;

        public StructBuilder(CompilerContext context, string ns, string name) : base(ns, name)
        {
            _name = name ?? throw new ArgumentNullException(nameof(name));
            _namespace = ns ?? "";
            _context = context ?? throw new ArgumentNullException(nameof(context));
            _typeValue = LLVM.StructCreateNamed(context.LLVMContext, string.IsNullOrWhiteSpace(ns) ? name : ns + "." + name);
        }

        public override TypeKind Kind => TypeKind.Struct;

        public override LLVMTypeRef TypeValue => _typeValue;
    }
}
