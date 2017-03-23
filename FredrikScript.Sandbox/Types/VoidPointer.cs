using LLVMSharp;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace FredrikScript.Sandbox.Types
{
    public class VoidPointer : IType
    {
        private readonly string _namespace;
        private readonly string _name;
        private readonly CompilerContext _context;
        private readonly LLVMTypeRef _structTypeValue;
        private readonly LLVMTypeRef _typeValue;

        public VoidPointer(CompilerContext context)
        {
            _context = context ?? throw new ArgumentNullException(nameof(context));
            var voidType = LLVM.Int8TypeInContext(context.LLVMContext);
            _typeValue = LLVM.PointerType(voidType, 0);
        }

        public string Namespace => "void*";
        public string Name => "void*";

        public TypeKind Kind => TypeKind.Pointer;

        public LLVMTypeRef TypeValue => _typeValue;

        public string FullyQualifiedName => "void*";
    }
}
