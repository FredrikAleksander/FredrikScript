using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using LLVMSharp;

namespace FredrikScript.Sandbox
{

    public interface IType
    {
        string FullyQualifiedName { get; }
        string Name { get; }
        string Namespace { get; }
        TypeKind Kind { get; }
        LLVMTypeRef TypeValue { get; }
    }
}
