using FredrikScript.Core.Types;
using LLVMSharp;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace FredrikScript.LLVMCodeGen.Types
{
    public class LLVMUshortType : UshortType, ILLVMType
    {
        WeakReference<LLVMContext> _context;
        LLVMTypeRef _llvmType;

        public LLVMUshortType(LLVMContext context)
        {
            _context = new WeakReference<LLVMContext>(context ?? throw new ArgumentNullException(nameof(context)));
        }

        public LLVMTypeRef LLVMType
        {
            get
            {
                if (_llvmType.Pointer == IntPtr.Zero && _context.TryGetTarget(out var context))
                    _llvmType = LLVM.Int16TypeInContext(context.LLVMHandle);
                return _llvmType;
            }
        }
    }
}
