using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace FredrikScript.Core.Types
{
    public class ArrayBuilder : TypeBuilder
    {
        public ArrayBuilder(IType elementType) : base(elementType.Namespace, elementType.Name + "[]")
        {
            ElementType = elementType ?? throw new ArgumentNullException(nameof(elementType));
        }

        public IType ElementType { get; }

        public override TypeKind Kind => TypeKind.Array;
    }
}
