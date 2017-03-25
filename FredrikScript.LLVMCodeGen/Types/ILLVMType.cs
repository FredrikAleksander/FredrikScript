using FredrikScript.Core.Types;
using LLVMSharp;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace FredrikScript.LLVMCodeGen.Types
{
    public interface ILLVMType : IType
    {
        LLVMTypeRef LLVMType { get; }
    }
}
