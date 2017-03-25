using FredrikScript.Core.Types;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using LLVMSharp;
using FredrikScript.Core;

namespace FredrikScript.LLVMCodeGen.Types
{
    public class LLVMClassBuilder : ClassBuilder, ILLVMType
    {
        private readonly WeakReference<LLVMContext> _context;
        private LLVMClassBuilder _baseClass;
        private readonly List<LLVMInterfaceBuilder> _interfaces = new List<LLVMInterfaceBuilder>();
        private LLVMTypeRef _llvmClassDescPointerType;
        private LLVMTypeRef _llvmStructType;
        private bool _checkedStructType;
        private LLVMTypeRef _llvmClassType; // ClassDesc Pointer + Struct Type
        private LLVMTypeRef _llvmType; // Pointer of ClassType

        public LLVMClassBuilder(LLVMContext context, string ns, string name) : base(ns, name)
        {
            _context = new WeakReference<LLVMContext>(context ?? throw new ArgumentNullException(nameof(context)));
        }

        public LLVMTypeRef LLVMStructType
        {
            get
            {
                if (!_checkedStructType)
                {
                    if (_context.TryGetTarget(out var context)) {

                        var fieldTypes = new List<LLVMTypeRef>();
                        if (_baseClass != null && _baseClass?.LLVMStructType.Pointer != IntPtr.Zero)
                            fieldTypes.Add(_baseClass.LLVMStructType);
                        fieldTypes.AddRange(Fields.Select(x => (x as ILLVMType).LLVMType).ToArray());

                        if (fieldTypes.Count > 0)
                            _llvmStructType = LLVM.StructTypeInContext(context.LLVMHandle, fieldTypes.ToArray(), false);

                        _checkedStructType = true;
                    }
                }
                return _llvmStructType;
            }
        }

        protected override MethodBuilder CreateMethodBuilder(SourceInformation sourceInformation, ModuleBuilder module, string item, string methodName, Visibility visibility, StorageClass storageClass, IType declaringType, IType returnType, IType[] parameterTypes, string[] parameterNames, string externString)
        {
            if(_context.TryGetTarget(out var context))
                return new LLVMMethodBuilder(context, sourceInformation, module, item, methodName, visibility, storageClass, declaringType, returnType, parameterTypes, parameterNames, externString);
            throw new InvalidOperationException();
        }

        public LLVMTypeRef LLVMType
        {
            get
            {
                if (_llvmType.Pointer == IntPtr.Zero && _context.TryGetTarget(out var context))
                {
                    _llvmClassType = LLVM.StructCreateNamed(context.LLVMHandle, FullyQualifiedName);
                    _llvmType = LLVM.PointerType(_llvmClassType, 0);
                }
                return _llvmType;
            }
        }

        public void UpdateLLVMType()
        {
            if (_context.TryGetTarget(out var context))
            {
                if (_llvmType.Pointer == IntPtr.Zero)
                {
                    _llvmClassType = LLVM.StructCreateNamed(context.LLVMHandle, FullyQualifiedName);
                    _llvmType = LLVM.PointerType(_llvmClassType, 0);
                }

                // TODO: Implement classdesc
                var dummyPointer = context.Types["void*"];
                _llvmClassDescPointerType = (dummyPointer as ILLVMType).LLVMType;
                var structType = LLVMStructType;
                var elements = new List<LLVMTypeRef>();
                elements.Add(_llvmClassDescPointerType);
                if (structType.Pointer != IntPtr.Zero)
                    elements.Add(structType);
                LLVM.StructSetBody(_llvmClassType, elements.ToArray(), false);
            }
        }
    }
}
