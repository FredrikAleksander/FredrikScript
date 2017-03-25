using FredrikScript.Core.Types;
using LLVMSharp;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace FredrikScript.LLVMCodeGen.Types
{
    public class LLVMModuleBuilder : ModuleBuilder
    {
        private LLVMModuleRef _module;
        private WeakReference<LLVMContext> _context;

        public LLVMModuleBuilder(LLVMContext context, string name) : base(name)
        {
            _context = new WeakReference<LLVMContext>(context ?? throw new ArgumentNullException(nameof(context)));
            _module = LLVM.ModuleCreateWithNameInContext(name, context.LLVMHandle);
        }

        public LLVMModuleRef LLVMModule => _module;
    }
}
