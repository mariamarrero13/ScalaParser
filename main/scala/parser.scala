import scala.util.parsing.combinator._

class Parser extends RegexParsers{
    val digit : Parser[Any] = "[0-9]".r
    val character: Parser[Any] = "[A-za-z_?]".r
    val delimiter: Parser[Any] = "[()\\[\\];,]".r
    val operator: Parser[Any] = "[\\+-~*/=(!=)<>(<=)(>=)&|(:=)]".r

    def Exp : Parser[Any] = Term~opt(Binop~exp)|"if" ~ Exp ~ "then" ~ Exp ~ "else" Exp | "let" ~ Def+ ~ "in" ~ Exp
    def Term : Parser[Any] = Unop~Term | Factor ~ opt(ExpList)| Empty | int | Bool 
    def Factor : Parser[Any] = Exp | Prim | Id
    def ExpList : Parser[Any] = PropExpList
    def PropExpList : Parser[Any] = Exp | Exp~","~PropExpList
    def IdList : Parser[Any] = PropIdList
    def PropIdList : Parser[Any] = Id | Id~","~ PropIdList
    def Def : Parser[Any] = Id = Exp
    def Empty : Parser[Any] = ""
    def Bool : Parser[Any] = true | false
    def Unop : Parser[Any] = Sign | "~"
    def Sign : Parser[Any] = "+"|"-"
    def Binop : Parser[Any] = Sign | "*" | "/" | "=" | "!=" | "<" | ">" | "<=" | ">=" | "&" | "|"
    def Prim : Parser[Any] = "number?" | "function?" | "list?" |"empty?" | "cons?" | "cons" | "first" | "rest" | 
    def Id : Parser[Any] = character ~ opt(Character|Digit)*
    def int : Parser[Any] = digit+

}
