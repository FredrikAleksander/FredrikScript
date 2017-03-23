using System;
using LLVMSharp;

namespace FredrikScript.Sandbox.Types
{
    public class UshortType : IType
    {
        private LLVMTypeRef _typeValue;

        public UshortType(CompilerContext context)
        {
            if (context == null) throw new ArgumentNullException(nameof(context));
            _typeValue = context.LLVMContext.Int16TypeInContext();
        }

        public TypeKind Kind => TypeKind.Ushort;

        public LLVMTypeRef TypeValue => _typeValue;

        public string FullyQualifiedName => "ushort";

        public string Name => "ushort";

        public string Namespace => "";

        public int InstanceSize => 2;
    }
}
