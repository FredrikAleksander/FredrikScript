using FredrikScript.Core.Types;
using LLVMSharp;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace FredrikScript.LLVMCodeGen.Types
{
    public class LLVMStructBuilder : StructBuilder
    {
        private readonly WeakReference<LLVMContext> _context;
        private LLVMTypeRef _llvmType;

        public LLVMStructBuilder(LLVMContext context, string ns, string name) : base(ns, name)
        {
            _context = new WeakReference<LLVMContext>(context ?? throw new ArgumentNullException(nameof(context)));
        }

        public LLVMTypeRef LLVMType
        {
            get
            {
                if (_llvmType.Pointer == IntPtr.Zero && _context.TryGetTarget(out var context))
                    _llvmType = LLVM.StructCreateNamed(context.LLVMHandle, FullyQualifiedName);
                return _llvmType;
            }
        }

        internal void UpdateLLVMType()
        {
            if(_context.TryGetTarget(out var context))
            {
                if (Fields.Count == 0)
                    throw new Exception("Structs must have atleast one field");
                if (_llvmType.Pointer == IntPtr.Zero)
                    _llvmType = LLVM.StructCreateNamed(context.LLVMHandle, FullyQualifiedName);
                var fieldTypes = Fields.Select(x => (x as ILLVMType).LLVMType).ToArray();
                _llvmType.StructSetBody(fieldTypes, false);
            }
        }
    }
}
