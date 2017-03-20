open Parser
open FParsec

let printSuccess x =
    printfn "%A\n\n" x
let printFailure x =
    printfn "%A\n\n" x

let test p input = 
    match run p input with
    | Success (r, _, _) -> printSuccess r
    | Failure (err, _, _) -> printFailure err

[<EntryPoint>]
let main argv = 
    test pCodeBlock "{return x.y.z.ToString(); }"
//    test pCompilationUnit "using System.Data;using System.Net;class FFF{}namespace System.IO { class ff{ int[] PrintData(object o) {return x(0, \"Hello World!\");}int PrintData2(object o) {return;} int y;} namespace Async{enum DaFgh { SOME_ID = 0, SOME_DATA=1, SOMEOTHER } }}"
//    test pCompilationUnit "using System.Data;using System.Net;class FFF{}namespace System.IO { class ff{ int[] PrintData(object o) {return x(0, \"Hello World!\");}int PrintData2(object o) {return;} int y;} namespace Async{enum DaFgh { SOME_ID = 0, SOME_DATA=1, SOMEOTHER } }}"
//    test pType "public interface IDisposable{void Dispose();} "
//    test pType "public enum TEst { DATAID=0,DATAID2,DATAID3=4 }"
//    test pType "public struct TEst { private constructor(int x) {} int x  ; void* y()  ;  extern \"ff\" void z() {return x.x.x.x.SomeMethod() + x + 100.0f;}}"
    //test pMember "constructor (int x, int y) {return 0; return                ;}"
    //test pMember "static int x() { return 0 ;}"
    //test pMember "static int x();"
    //test pMember "int x;"

    printfn "%A" argv
    0 // return an integer exit code
