using LLVMSharp;
using System;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace FredrikScript.Sandbox
{
    class Program
    {
        private static Ast.CompilationUnit[] ParseFiles(string[] files)
        {
            var results = new System.Collections.Concurrent.ConcurrentBag<Ast.CompilationUnit>();
            Parallel.ForEach(files, (currentFile) => results.Add(Parser.parseFile(currentFile, Encoding.UTF8)));
            return results.ToArray();
        }

        static void Main(string[] files)
        {
            var parsed = ParseFiles(files);
            var llvmCompiler = new CompilerContext();
            var visitorPass1 = new TypeVisitorPass1(llvmCompiler);
            var visitorPass2 = new TypeVisitorPass2(llvmCompiler);
            foreach(var cu in parsed)
                visitorPass1.VisitCompilationUnit(cu);
            foreach (var cu in parsed)
                visitorPass2.VisitCompilationUnit(cu);
            Console.WriteLine("Done!");
        }
    }
}
