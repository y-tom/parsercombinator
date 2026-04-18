package jp.ed.nnn.parsercombinator

abstract class Combinator {
  // === 文字列パーサs === MyFirstCombinatorと同じ
  sealed trait ParseResult[+T] // パース結果の型
  case class Success[+T](value: T, next: String) extends ParseResult[T] // 成功（読み取った値,残りの文字列)
  case object Failure extends ParseResult[Nothing] // 失敗

  // --- Parserの定義 文字列を受け取って、成功か失敗を返す関数 ---
  type Parser[+T] = String => ParseResult[T]

  // 先頭がliteralなら成功で、呼んだ文字列を返し、残りも返す 先頭がliteralでなければ失敗
  def string(literal: String): Parser[String] = input => {
    if (input.startsWith(literal)) {
      Success(literal, input.substring(literal.length))
    } else {
      Failure
    }
  }

  // string を s と短く呼ぶための別名メソッド
  /**
   * string parser
   * @param literal 文字列
   * @return
   */
  def s(literal: String): Parser[String] = string(literal)

  // === 選択 ===
  extension [T](parser: Parser[T])
    /**
     * select
     *
     * @param right 選択を行うパーサー
     * @return
     */
    def |[U >: T](right: Parser[U]): Parser[U] = input =>
      parser(input) match
        case success@Success(_, _) => success
        case Failure => right(input)

    // === 逐次合成 ===
    /**
     * combine
     *
     * @param right 逐次合成を行うパーサー
     * @tparam U パーサーの結果の型
     * @return
     */
    def ~[U](right: Parser[U]): Parser[(T, U)] = input => {
      parser(input) match {
        case Success(value1, next1) =>
          right(next1) match {
            case Success(value2, next2) =>
              Success((value1, value2), next2)
            case Failure =>
              Failure
          }
        case Failure =>
          Failure
      }
    }
  // === 部分取得 ===
    /**
     * use left
     *
     * @param right 右側のパーサー
     * @return パーサーの結果の型
     */
    def <~(right: Parser[Any]): Parser[T] = input => {
      parser(input) match {
        case Success(value1, next1) =>
          right(next1) match {
            case Success(value2, next2) =>
              Success(value1, next2)
            case Failure =>
              Failure
          }
        case Failure =>
          Failure
      }
    }

    /**
     * use right
     *
     * @param right 右側のパーサー
     * @tparam U パーサーの結果の型
     * @return
     */
    def ~>[U](right: Parser[U]): Parser[U] = input => {
      parser(input) match {
        case Success(value1, next1) =>
          right(next1) match {
            case Success(value2, next2) =>
              Success(value2, next2)
            case Failure =>
              Failure
          }
        case Failure =>
          Failure
      }
    }

  // === 変換 ===
    /**
     * map
     *
     * @param function 適用する関数
     * @tparam U パーサーの結果の型
     * @return
     */
    def ^^[U](function: T => U): Parser[U] = input => {
      parser(input) match {
        case Success(value, next) => Success(function(value), next)
        case Failure => Failure
      }
    }

}

