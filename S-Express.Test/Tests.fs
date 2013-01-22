namespace SExpress.Test

open SExpress
open FsUnit
open NUnit.Framework

module Tests =

  let testParseAndPrint item =
    fun () ->
      let parsed = item |> SExpressions.parse_ic
      let reprint = parsed |> SExpressions.print_sexpr 
      let reprintIndent = parsed |> SExpressions.print_sexpr_indent
      printfn "%A\n" parsed
      printfn "%A\n" reprint
      printfn "%A\n" reprintIndent
      


  [<Test>]
  let testParser() =
    let items = 
      [
        "()";
        "(test string)";
        "(test)";
        "(test \"internal string\")";
        "(defun factorial (x)
          (if (zerop x)
            1
            (* x (factorial (- x 1)))))";
         "((a b) (c d))";
         "((data \"quoted data\" 123 4.5)
           (data (!@# (4.5) \"(more\" \"data)\")))";
         "(block \"the Name\"
            (inputs (a 'a) (b 'b) (c 'c))
            (outputs a b c))";
            "(((S) (NP VP))
             ((VP) (V))
             ((VP) (V NP))
             ((V) died)
             ((V) employed)
             ((NP) nurses)
             ((NP) patients)
             ((NP) Medicenter)
             ((NP) \"Dr Chan\"))"
      ]
    
    items 
    |> List.iter (fun x -> Assert.DoesNotThrow(TestDelegate(testParseAndPrint(x))))
  
    
