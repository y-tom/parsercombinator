package jp.ed.nnn.parsercombinator

object JSONParser extends Combinator {

  def obj: Parser[Map[String, Any]]  =
    s("{") ~> repsep(member, s(",")) <~ s("}") ^^ { Map() ++ _ }

  def arr: Parser[List[Any]] =
    s("[") ~> repsep(value, s(",")) <~ s("]")

  def member: Parser[(String, Any)] =
    stringLiteral ~ s(":") ~ value ^^ { t => (t._1._1, t._2) }

  def value: Parser[Any] =
    obj |
      arr |
      stringLiteral |
      (floatingPointNumber ^^ { _.toDouble }) |
      s("null") ^^  { _ => null } |
      s("true") ^^  { _ => true } |
      s("false") ^^  { _ => false }

  def apply(input: String): Any = value(input)

}