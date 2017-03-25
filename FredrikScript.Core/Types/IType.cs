using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace FredrikScript.Core.Types
{

    public interface IType
    {
        string FullyQualifiedName { get; }
        string Name { get; }
        string Namespace { get; }
        TypeKind Kind { get; }
    }
}
