using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using LLVMSharp;
using System.Runtime.InteropServices;
using FredrikScript.LLVMCodeGen;
using FredrikScript.LLVMCodeGen.Types;

namespace FredrikScript.JitExecutor
{
    class Program
    {
        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MainFunction(int x);

        static void Main(string[] args)
        {
            string moduleName = "dummy";

            LLVMContext context = new LLVMContext(moduleName);
            var parser = new Parser.Parser();
            var compiler = new Compiler();
            parser.Parse(context, args);
            compiler.Compile(context);

            IntPtr error = IntPtr.Zero;
            LLVMExecutionEngineRef engine;
            LLVM.LinkInMCJIT();
            LLVM.InitializeNativeTarget();
            LLVM.InitializeNativeAsmPrinter();

            var options = new LLVMMCJITCompilerOptions();
            var optionsSize = (4 * sizeof(int)) + IntPtr.Size; // LLVMMCJITCompilerOptions has 4 ints and a pointer

            LLVM.InitializeMCJITCompilerOptions(out options, optionsSize);
            LLVM.CreateMCJITCompilerForModule(out engine, (context.ModuleBuilder as LLVMModuleBuilder).LLVMModule, out options, optionsSize, out error);

            var entryPoint = (MainFunction)Marshal.GetDelegateForFunctionPointer(LLVM.GetPointerToGlobal(engine, context.GetEntryPointLLVM()), typeof(MainFunction));
            int result = entryPoint(100);

            Console.WriteLine("Exit Code: " + result);

            if (LLVM.WriteBitcodeToFile((context.ModuleBuilder as LLVMModuleBuilder).LLVMModule, $"{moduleName}.bc") != 0)
            {
                Console.WriteLine("error writing bitcode to file, skipping");
            }

            LLVM.DumpModule((context.ModuleBuilder as LLVMModuleBuilder).LLVMModule);
            LLVM.DisposeExecutionEngine(engine);
            Console.ReadKey();
        }
    }
}
