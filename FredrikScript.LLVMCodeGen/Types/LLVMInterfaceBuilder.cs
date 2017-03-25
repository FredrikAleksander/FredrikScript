using FredrikScript.Core;
using FredrikScript.Core.Types;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using LLVMSharp;

namespace FredrikScript.LLVMCodeGen.Types
{
    public class LLVMInterfaceBuilder : InterfaceBuilder, ILLVMType
    {
        private WeakReference<LLVMContext> _context;
        private LLVMTypeRef _llvmType;

        public LLVMInterfaceBuilder(LLVMContext context, string ns, string name) : base(ns, name)
        {
            _context = new WeakReference<LLVMContext>(context ?? throw new ArgumentNullException(nameof(context)));
        }

        public LLVMTypeRef LLVMType
        {
            get
            {
                if(_llvmType.Pointer == IntPtr.Zero && _context.TryGetTarget(out var context))
                {
                    // TODO: Create Virtual Function Table struct
                    var intType = LLVM.Int32TypeInContext(context.LLVMHandle);
                    _llvmType = LLVM.StructTypeInContext(context.LLVMHandle, new LLVMTypeRef[] { intType, intType }, false);
                }
                return _llvmType;
            }
        }
    }
}
