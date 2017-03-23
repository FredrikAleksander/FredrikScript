using System;
using LLVMSharp;

namespace FredrikScript.Sandbox.Types
{
    public class DoubleType : IType
    {
        private LLVMTypeRef _typeValue;

        public DoubleType(CompilerContext context)
        {
            if (context == null) throw new ArgumentNullException(nameof(context));
            _typeValue = context.LLVMContext.DoubleTypeInContext();
        }

        public TypeKind Kind => TypeKind.Double;

        public LLVMTypeRef TypeValue => _typeValue;

        public string FullyQualifiedName => "double";

        public string Name => "double";

        public string Namespace => "";
    }
}
