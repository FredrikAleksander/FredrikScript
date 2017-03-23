using System;
using LLVMSharp;

namespace FredrikScript.Sandbox.TypeBuilders
{
    public class EnumBuilder : TypeBuilder, IType
    {
        private readonly string _namespace;
        private readonly string _name;
        private readonly CompilerContext _context;
        private readonly LLVMTypeRef _typeValue;

        public EnumBuilder(CompilerContext context, string ns, string name)
        {
            _context = context ?? throw new ArgumentNullException(nameof(context));
            _namespace = ns ?? "";
            _name = name ?? throw new ArgumentNullException(nameof(name));
            _typeValue = LLVM.Int32TypeInContext(context.LLVMContext);
        }

        public override string Namespace => _namespace;
        public override string Name => _name;

        public override TypeKind Kind => TypeKind.Enum;

        public override LLVMTypeRef TypeValue => _typeValue;
    }
}
