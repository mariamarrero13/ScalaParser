import scala.util.parsing.combinator._

class CICOMparser extends RegexParsers{
    val digit : Parser[Any] = "[0-9]".r
    val character: Parser[Any] = "[A-za-z_?]".r
    val delimiter: Parser[Any] = "[()\\[\\];,]".r
    val operator: Parser[Any] = "[\\+-~*/=(!=)<>(<=)(>=)&|(:=)]".r

    def Exp : Parser[Any] = "if"~Exp ~"then" ~ Exp ~ "else" ~ Exp | "let"~ rep1(Def)~"in"~ Exp |"map" ~ IdList ~ "to" ~ Exp|Term~rep(Binop~Exp)
    def Term : Parser[Any] = Unop~Term | Factor ~ rep( "(" ~ExpList~ ")")| Empty | int | Bool 
    def Factor : Parser[Any] = "(" ~ Exp ~ ")" | Prim | Id
    def ExpList : Parser[Any] = rep(PropExpList)
    def PropExpList : Parser[Any] = Exp~","~PropExpList|Exp 
    def IdList : Parser[Any] = rep(PropIdList)
    def PropIdList : Parser[Any] = Id~","~ PropIdList |Id 
    def Def : Parser[Any] = Id ~ ":=" ~ Exp ~ ";"
    def Empty : Parser[Any] = "empty"
    def Bool : Parser[Any] = "true" | "false"
    def Unop : Parser[Any] = Sign | "~"
    def Sign : Parser[Any] = "+" | "-"
    def Binop : Parser[Any] = Sign | "*" | "/" | "=" | "!=" |  "<=" | ">=" |"<" | ">" | "&" | "|"
    def Prim : Parser[Any] = "number?" | "function?" | "list?" |"empty?" | "cons?" | "cons" | "first" | "rest" | "arity"
    def Id : Parser[Any] = character ~ rep((character|digit)~"*")
    def int : Parser[Any] = rep1(digit)
}
