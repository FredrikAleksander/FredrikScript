using FredrikScript.Core.Types;
using LLVMSharp;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace FredrikScript.LLVMCodeGen.Types
{
    public class LLVMStringType : StringType, ILLVMType
    {
        WeakReference<LLVMContext> _context;
        LLVMTypeRef _llvmCharType;
        LLVMTypeRef _llvmCharArrayType;
        LLVMTypeRef _llvmSizeType;
        LLVMTypeRef _llvmType;
        private LLVMTypeRef _llvmStructType;

        public LLVMStringType(LLVMContext context)
        {
            _context = new WeakReference<LLVMContext>(context ?? throw new ArgumentNullException(nameof(context)));
        }

        public LLVMTypeRef LLVMType
        {
            get
            {
                if (_llvmType.Pointer == IntPtr.Zero && _context.TryGetTarget(out var context))
                {
                    _llvmCharType = LLVM.Int8TypeInContext(context.LLVMHandle);
                    _llvmCharArrayType = LLVM.ArrayType(_llvmCharType, 1);
                    _llvmSizeType = LLVM.Int64TypeInContext(context.LLVMHandle);
                    _llvmStructType = LLVM.StructTypeInContext(context.LLVMHandle, new LLVMTypeRef[] { _llvmSizeType, _llvmCharArrayType }, false);
                    _llvmType = LLVM.PointerType(_llvmStructType, 0);
                }
                return _llvmType;
            }
        }
    }
}
