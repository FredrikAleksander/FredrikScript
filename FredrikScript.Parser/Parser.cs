using FredrikScript.Core;
using FredrikScript.ParserFS;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace FredrikScript.Parser
{
    public class Parser
    {
        private Ast.CompilationUnit[] ParseFiles(string[] files)
        {
            var results = new System.Collections.Concurrent.ConcurrentBag<Ast.CompilationUnit>();
            Parallel.ForEach(files, (currentFile) => results.Add(ParserFS.Parser.parseFile(currentFile, Encoding.UTF8)));
            return results.ToArray();
        }
        public void Parse(Context context, string[] sourceFiles)
        {
            var parsed = ParseFiles(sourceFiles);
            var visitorPass1 = new TypeVisitorPass1(context);
            var visitorPass2 = new TypeVisitorPass2(context);
            var visitorPass3 = new ParseExpressionVisitor(context);
            foreach (var cu in parsed)
                visitorPass1.Visit(cu);
            foreach (var cu in parsed)
                visitorPass2.Visit(cu);
            foreach (var cu in parsed)
                visitorPass3.Visit(cu);
        }
    }
}
