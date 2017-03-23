using System;
using LLVMSharp;

namespace FredrikScript.Sandbox.Types
{
    public class UintType : IType
    {
        private LLVMTypeRef _typeValue;

        public UintType(CompilerContext context)
        {
            if (context == null) throw new ArgumentNullException(nameof(context));
            _typeValue = context.LLVMContext.Int32TypeInContext();
        }

        public TypeKind Kind => TypeKind.Uint;

        public LLVMTypeRef TypeValue => _typeValue;

        public string FullyQualifiedName => "uint";

        public string Name => "uint";

        public string Namespace => "";
    }
}
