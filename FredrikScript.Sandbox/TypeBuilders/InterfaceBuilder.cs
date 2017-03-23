using System;
using LLVMSharp;

namespace FredrikScript.Sandbox.TypeBuilders
{
    public class InterfaceBuilder : TypeBuilder, IType
    {
        private readonly string _namespace;
        private readonly string _name;
        private readonly CompilerContext _context;
        private readonly LLVMTypeRef _structTypeValue;
        private readonly LLVMTypeRef _typeValue;

        public InterfaceBuilder(CompilerContext context, string ns, string name) : base(ns, name)
        {
            _name = name ?? throw new ArgumentNullException(nameof(name));
            _namespace = ns ?? "";
            _context = context ?? throw new ArgumentNullException(nameof(context));
            _structTypeValue = LLVM.StructCreateNamed(context.LLVMContext, string.IsNullOrWhiteSpace(ns) ? name : ns + "." + name);
            _typeValue = LLVM.PointerType(_structTypeValue, 0);
        }

        public override TypeKind Kind => TypeKind.Interface;

        public override LLVMTypeRef TypeValue => _typeValue;
    }
}
