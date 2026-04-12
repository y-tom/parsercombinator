package jp.ed.nnn.parsercombinator

object BooleanParser extends MyFirstCombinator {

  // --- パーサコンビネーターライブラリのsとmapを利用して、trueParserとfalseParserを実装
  def trueParser: Parser[Boolean] = map(s("true"), (_: String) => true)
  def falseParser: Parser[Boolean] = map(s("false"), (_: String) => false)

  // --- select という選択するコンビネーターを利用してパーサを合成 ---
  def apply(input: String): ParseResult[Boolean] =
    select(trueParser, falseParser)(input)
}