using FredrikScript.Core;
using LLVMSharp;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using FredrikScript.Core.Types;
using FredrikScript.LLVMCodeGen.Types;

namespace FredrikScript.LLVMCodeGen
{
    public class LLVMContext : Context, IDisposable
    {
        private LLVMContextRef _context;
        private LLVMBuilderRef _builder;

        public LLVMContext(string moduleName) : base(moduleName)
        {
            _context = LLVM.ContextCreate();
            _builder = LLVM.CreateBuilderInContext(_context);
        }

        public LLVMContextRef LLVMHandle => _context;
        public LLVMBuilderRef LLVMBuilder => _builder;

        protected override ClassBuilder CreateClassBuilder(string @namespace, string name) { return new LLVMClassBuilder(this, @namespace, name); }
        protected override StructBuilder CreateStructBuilder(string @namespace, string name) { return new LLVMStructBuilder(this, @namespace, name); }
        protected override ArrayBuilder CreateArray(IType elementType) { return new LLVMArrayBuilder(this, elementType); }
        protected override ByteType CreateByteType() { return new LLVMByteType(this); }
        protected override SbyteType CreateSbyteType() { return new LLVMSbyteType(this); }
        protected override DoubleType CreateDoubleType() { return new LLVMDoubleType(this); }
        protected override FloatType CreateFloatType() { return new LLVMFloatType(this); }
        protected override IntType CreateIntType() { return new LLVMIntType(this); }
        protected override LongType CreateLongType() { return new LLVMLongType(this); }
        protected override ShortType CreateShortType() { return new LLVMShortType(this); }
        protected override StringType CreateStringType() { return new LLVMStringType(this); }

        protected override BooleanType CreateBooleanType() { return new LLVMBooleanType(this); }
        protected override CharType CreateCharType() { return new LLVMCharType(this); }
        protected override UintType CreateUintType() { return new LLVMUintType(this); }
        protected override UlongType CreateUlongType() { return new LLVMUlongType(this); }
        protected override UshortType CreateUshortType() { return new LLVMUshortType(this); }
        protected override VoidPointerType CreateVoidPointer() { return new LLVMVoidPointerType(this); }
        protected override VoidType CreateVoidType() { return new LLVMVoidType(this); }

        protected override ModuleBuilder CreateModule(string moduleName)
        {
            return new LLVMModuleBuilder(this, moduleName);
        }

        public LLVMValueRef GetEntryPointLLVM(string name = null)
        {
            var entryPoint = GetEntryPoint(name) ?? throw new Exception($"Could not find entry point '{name}'");
            return LLVM.GetNamedFunction((ModuleBuilder as LLVMModuleBuilder).LLVMModule, entryPoint.SymbolName);
        }

        #region IDisposable Support
        private bool disposedValue = false; // To detect redundant calls

        protected virtual void Dispose(bool disposing)
        {
            if (!disposedValue)
            {
                _context.ContextDispose();
                _context.Pointer = IntPtr.Zero;
                disposedValue = true;
            }
        }
        ~LLVMContext()
        {
            // Do not change this code. Put cleanup code in Dispose(bool disposing) above.
            Dispose(false);
        }

        // This code added to correctly implement the disposable pattern.
        public void Dispose()
        {
            Dispose(true);
            GC.SuppressFinalize(this);
        }
        #endregion
    }
}
