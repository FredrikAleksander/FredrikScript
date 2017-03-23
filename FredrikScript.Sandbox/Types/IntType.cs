using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using LLVMSharp;

namespace FredrikScript.Sandbox.Types
{
    public class IntType : IType
    {
        private LLVMTypeRef _typeValue;

        public IntType(CompilerContext context)
        {
            if (context == null) throw new ArgumentNullException(nameof(context));
            _typeValue = context.LLVMContext.Int32TypeInContext();
        }

        public TypeKind Kind => TypeKind.Int;

        public LLVMTypeRef TypeValue => _typeValue;

        public string FullyQualifiedName => "int";

        public string Name => "int";

        public string Namespace => "";
    }
}
