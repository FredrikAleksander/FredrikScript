open FredrikScript
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
    //test pInheritance ": object"
    test pCompilationUnit "enum TestClass { SomeId = 0 }"
    test pCompilationUnit "internal enum SeekDirection { Set, Cur, End }"
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

    printfn "%A" argv
    0 // return an integer exit code
