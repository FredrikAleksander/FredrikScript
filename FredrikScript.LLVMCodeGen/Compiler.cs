using FredrikScript.Core;
using FredrikScript.Core.Types;
using FredrikScript.LLVMCodeGen.Types;
using LLVMSharp;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace FredrikScript.LLVMCodeGen
{
    public class Compiler
    {
        public void Compile(LLVMContext context)
        {
            foreach(var type in context.Types)
                if(type.Value is ClassBuilder)
                    (type.Value as LLVMClassBuilder).UpdateLLVMType();
                else if (type.Value is StructBuilder)
                    (type.Value as LLVMStructBuilder).UpdateLLVMType();
            foreach (var type in context.Types)
                if (type.Value is ClassBuilder)
                    foreach (var method in (type.Value as ClassBuilder).Methods)
                        CompileMethod(context, method as LLVMMethodBuilder);
                else if (type.Value is StructBuilder)
                    foreach (var method in (type.Value as StructBuilder).Methods)
                        CompileMethod(context, method as LLVMMethodBuilder);
            IntPtr error;
            LLVM.VerifyModule((context.ModuleBuilder as LLVMModuleBuilder).LLVMModule, LLVMVerifierFailureAction.LLVMAbortProcessAction, out error);
            LLVM.DisposeMessage(error);
        }

        private void CompileMethod(LLVMContext context, LLVMMethodBuilder method)
        {
            var codeGenVisitor = new LLVMCodeGenVisitor(context, method);
            codeGenVisitor.Apply();
        }
    }
}
