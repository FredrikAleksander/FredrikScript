﻿using FredrikScript.Core.Types;
using LLVMSharp;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace FredrikScript.LLVMCodeGen.Types
{
    public class LLVMFloatType : FloatType, ILLVMType
    {
        WeakReference<LLVMContext> _context;
        LLVMTypeRef _llvmType;

        public LLVMFloatType(LLVMContext context)
        {
            _context = new WeakReference<LLVMContext>(context ?? throw new ArgumentNullException(nameof(context)));
        }

        public LLVMTypeRef LLVMType
        {
            get
            {
                if (_llvmType.Pointer == IntPtr.Zero && _context.TryGetTarget(out var context))
                    _llvmType = LLVM.FloatTypeInContext(context.LLVMHandle);
                return _llvmType;
            }
        }
    }
}
