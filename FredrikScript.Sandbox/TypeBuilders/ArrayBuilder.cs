using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using LLVMSharp;

namespace FredrikScript.Sandbox.TypeBuilders
{
    public class ArrayBuilder : TypeBuilder
    {
        private IType _elementType;
        private LLVMTypeRef _arrayType;
        private LLVMTypeRef _typeValue;

        public ArrayBuilder(CompilerContext context, IType elementType) : base(elementType.Namespace, elementType.Name + "[]")
        {
            _elementType = elementType ?? throw new ArgumentNullException(nameof(elementType));
            _arrayType = LLVM.ArrayType(_elementType.TypeValue, 1);
            LLVMTypeRef longType = LLVM.Int64TypeInContext(context.LLVMContext);
            _typeValue = LLVM.StructTypeInContext(context.LLVMContext, new LLVMTypeRef[] { longType, _arrayType }, false);
        }

        public IType ElementType => _elementType;

        public override TypeKind Kind => TypeKind.Array;

        public override LLVMTypeRef TypeValue => _typeValue;
    }
}
