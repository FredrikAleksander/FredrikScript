using FredrikScript.Core.Types;
using LLVMSharp;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace FredrikScript.LLVMCodeGen.Types
{
    public class LLVMArrayBuilder : ArrayBuilder, ILLVMType
    {
        WeakReference<LLVMContext> _context;
        LLVMTypeRef _llvmStructType;
        LLVMTypeRef _llvmType;

        public LLVMArrayBuilder(LLVMContext context, IType type) : base(type)
        {
            _context = new WeakReference<LLVMContext>(context ?? throw new ArgumentNullException(nameof(context)));
            if (!(type is ILLVMType))
                throw new ArgumentException("Type must be a LLVM type", nameof(type));
        }

        public LLVMTypeRef LLVMType
        {
            get
            {
                if (_llvmType.Pointer == IntPtr.Zero && _context.TryGetTarget(out var context))
                {
                    var elementType = (ElementType as ILLVMType).LLVMType;
                    var sizeType = LLVM.Int64TypeInContext(context.LLVMHandle);
                    _llvmStructType = LLVM.StructTypeInContext(context.LLVMHandle, new LLVMTypeRef[] { sizeType, elementType }, false);
                    _llvmType = LLVM.PointerType(_llvmStructType, 0);
                }
                return _llvmType;
            }
        }
    }
}
