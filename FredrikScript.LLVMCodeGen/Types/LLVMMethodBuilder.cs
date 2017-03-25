using FredrikScript.Core.Types;
using LLVMSharp;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using FredrikScript.Core;

namespace FredrikScript.LLVMCodeGen.Types
{
    public class LLVMMethodBuilder : MethodBuilder
    {
        private readonly WeakReference<LLVMContext> _context;
        private LLVMTypeRef _llvmFunctionType;
        private LLVMValueRef _llvmFunction;
        

        public LLVMMethodBuilder(LLVMContext context, SourceInformation sourceInformation, ModuleBuilder module, string name, string symbolName, Visibility visibility, StorageClass storageClass, IType declaringType, IType returnType, IType[] parameterTypes, string[] parameterNames, string externString) : base(sourceInformation, module, name, symbolName, visibility, storageClass, declaringType, returnType, parameterTypes, parameterNames, externString)
        {
            _context = new WeakReference<LLVMContext>(context);
        }

        

        public LLVMValueRef LLVMFunction
        {
            get
            {
                if(_llvmFunction.Pointer == IntPtr.Zero && _context.TryGetTarget(out var context))
                {
                    _llvmFunctionType = LLVM.FunctionType((ReturnType as ILLVMType).LLVMType,
                        ParameterTypes.Select(x => (x as ILLVMType).LLVMType).ToArray(), false);
                    _llvmFunction = LLVM.AddFunction((context.ModuleBuilder as LLVMModuleBuilder).LLVMModule, SymbolName, _llvmFunctionType);
                }
                return _llvmFunction;
            }
        }
    }
}
