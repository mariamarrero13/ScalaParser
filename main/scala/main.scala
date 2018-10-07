import scala.io._

object console {
    
      def main(args: Array[String]): Unit = {
       // var strToParse = "let\n  f := map n to if n = 0 then 1 else n * f(n - 1);\nin\n  let\n    f := map n,m,k to if (n <= 0 & n >= 0)\n                  | (n < 0 & n > 0 & n != 0) then number?\n                                           else m / f(k + 1);\n  in\n     let x:=3;\n         y:=4;\n         z:=cons?(function?(x * ~y), cons(-arity(x)));\n     in\n        let x:=3;\n            y:=4;\n            z:=g();\n        in\n            (g(x,y,z))(null?(true),list?(false),first(null))"
        var strToParse = scala.io.Source.fromFile("testprogram.txt").mkString
        println(strToParse)
        val parser = new CICOMparser
        val result = parser.parseAll(parser.Exp, strToParse)
        println(result.toString)
        }
  
}
