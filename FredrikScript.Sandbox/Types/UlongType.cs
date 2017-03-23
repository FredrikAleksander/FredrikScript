using System;
using LLVMSharp;

namespace FredrikScript.Sandbox.Types
{
    public class UlongType : IType
    {
        private LLVMTypeRef _typeValue;

        public UlongType(CompilerContext context)
        {
            if (context == null) throw new ArgumentNullException(nameof(context));
            _typeValue = context.LLVMContext.Int64TypeInContext();
        }

        public TypeKind Kind => TypeKind.Ulong;

        public LLVMTypeRef TypeValue => _typeValue;

        public string FullyQualifiedName => "ulong";

        public string Name => "ulong";

        public string Namespace => "";
    }
}
