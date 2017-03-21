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
    //test pTypeName "namespace"
    //test pIdentifier "namespace"
    //test pExpression "100L"
    test pTypeName "void*[][][0]"
    test pCodeBlock """{
    new int[][0];
    new FileStream("data.dat");
    for(var i : int = 0; i < 10; i++) {
        DoSomething();
    }
    if(x > 0) {
    }
    else if(y > x) {
        DoSomething();
    }
    else {
    }
    while(x > 0) { DoSomething(); }
    var x : int = y ^= 10 * 10;

    do { DoSomething(); } while(x > 0)
    do DoSomething(); while(x > 0)
    foreach(var x in y) {
        printf("heisann");
    }
    }"""
    test pStatement """while(x > 0) { DoSomething(); }"""
    test pStatement "blkblf;"
    test pExpression "truer[0].x[y + false].ToString()"
    //test pCodeBlock "{var x; var y : int[][] = \"\";  (x+y)*150-10; return (x +y)[0].y.z.ToString(); }"
    //test pCompilationUnit "using System.Data;using System.Net;class FFF{}namespace System.IO { class ff{ int[] PrintData(object o) {return x(0, \"Hello World!\");}int PrintData2(object o) {return;} int y;} namespace Async{enum DaFgh { SOME_ID = 0, SOME_DATA=1, SOMEOTHER } }}"
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
