using FredrikScript.Core.Types;
using LLVMSharp;
using System;

namespace FredrikScript.LLVMCodeGen.Types
{
    public class LLVMUlongType : UlongType, ILLVMType
    {
        WeakReference<LLVMContext> _context;
        LLVMTypeRef _llvmType;

        public LLVMUlongType(LLVMContext context)
        {
            _context = new WeakReference<LLVMContext>(context ?? throw new ArgumentNullException(nameof(context)));
        }

        public LLVMTypeRef LLVMType
        {
            get
            {
                if (_llvmType.Pointer == IntPtr.Zero && _context.TryGetTarget(out var context))
                    _llvmType = LLVM.Int64TypeInContext(context.LLVMHandle);
                return _llvmType;
            }
        }
    }
}
