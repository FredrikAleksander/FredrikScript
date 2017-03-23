using System;
using LLVMSharp;

namespace FredrikScript.Sandbox.Types
{
    public class FloatType : IType
    {
        private LLVMTypeRef _typeValue;

        public FloatType(CompilerContext context)
        {
            if (context == null) throw new ArgumentNullException(nameof(context));
            _typeValue = context.LLVMContext.FloatTypeInContext();
        }

        public TypeKind Kind => TypeKind.Float;

        public LLVMTypeRef TypeValue => _typeValue;

        public string FullyQualifiedName => "float";

        public string Name => "float";

        public string Namespace => "";
    }
}
