using System;
using LLVMSharp;

namespace FredrikScript.Sandbox.Types
{
    public class LongType : IType
    {
        private LLVMTypeRef _typeValue;

        public LongType(CompilerContext context)
        {
            if (context == null) throw new ArgumentNullException(nameof(context));
            _typeValue = context.LLVMContext.Int64TypeInContext();
        }

        public TypeKind Kind => TypeKind.Ulong;

        public LLVMTypeRef TypeValue => _typeValue;

        public string FullyQualifiedName => "long";

        public string Name => "long";

        public string Namespace => "";
    }
}
