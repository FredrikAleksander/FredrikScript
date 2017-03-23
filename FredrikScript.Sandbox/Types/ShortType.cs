using System;
using LLVMSharp;

namespace FredrikScript.Sandbox.Types
{
    public class ShortType : IType
    {
        private LLVMTypeRef _typeValue;

        public ShortType(CompilerContext context)
        {
            if (context == null) throw new ArgumentNullException(nameof(context));
            _typeValue = context.LLVMContext.Int16TypeInContext();
        }

        public TypeKind Kind => TypeKind.Short;

        public LLVMTypeRef TypeValue => _typeValue;

        public string FullyQualifiedName => "short";

        public string Name => "short";

        public string Namespace => "";
    }
}
