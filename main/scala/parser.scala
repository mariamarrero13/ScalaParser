import scala.util.parsing.combinator._

class CICOMparser extends RegexParsers{
    def Digits : Parser[Any] = "[0-9]".r
    def Characters: Parser[Any] = "[A-za-z_?]".r
    def Exp : Parser[Any] =  "let"~ rep1(Def)~"in"~ Exp |"map"~ IdList~"to" ~ Exp|"if"~Exp~"then"~Exp~"else"~Exp |Term~rep(Binop~Exp)
    def Term : Parser[Any] =  Empty | Intg | Bool |Factor ~ rep( "(" ~ExpList~ ")")|Unop~Term 
    def Factor : Parser[Any] = "(" ~ Exp ~ ")" | Prim | Id
    def ExpList : Parser[Any] = rep(PropExpList)
    def PropExpList : Parser[Any] = Exp~","~PropExpList |Exp
    def IdList : Parser[Any] = rep(PropIdList)
    def PropIdList : Parser[Any] =Id~","~ PropIdList|Id
    def Def : Parser[Any] = Id ~ ":=" ~ Exp ~";"
    def Empty : Parser[Any] = "empty"
    def Bool : Parser[Any] = "true" | "false"
    def Unop : Parser[Any] = Sign | "~"
    def Sign : Parser[Any] = "+" | "-"
    def Binop : Parser[Any] = Sign | "*" | "/" |  "!="| "=" |  "<=" | ">=" |"<" | ">" | "&" | "|"
    def Prim : Parser[Any] = "number?" | "function?" | "list?" |"empty?" | "cons?" | "cons" | "first" | "rest" | "arity"
    def Id : Parser[Any] = not(resKey) ~> Characters ~ rep(not(resKey)~> Characters|Digits)
    def Intg : Parser[Any] = rep1(Digits)
    def resKey :Parser[Any]= "let" | "in" | "map"  |"if" |"then" |"else" |"to"|"empty"|"true"|"false"| "number?" | "function?" | "list?" |"empty?" | "cons?" | "cons" | "first" | "rest" | "arity"
    def apply(input: String): Unit = parseAll(Exp, input) match {
    case Success(result, _) => println("Parsing Successful")
    case failure : NoSuccess => scala.sys.error(failure.msg)
  }
}
