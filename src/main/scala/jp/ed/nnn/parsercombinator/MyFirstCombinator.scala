package jp.ed.nnn.parsercombinator

abstract class MyFirstCombinator {

  // --- ParserStudyと同じ ---
  sealed trait ParseResult[+T] // パース結果の型
  case class Success[+T](value: T, next: String) extends ParseResult[T] // 成功（読み取った値,残りの文字列)
  case object Failure extends ParseResult[Nothing] // 失敗

  type Parser[+T] = String => ParseResult[T] // 文字列を受け取って、成功か失敗を返す関数

  // 先頭がliteralなら成功で、呼んだ文字列を返し、残りも返す 先頭がliteralでなければ失敗
  def string(literal: String): Parser[String] = input => {
    if(input.startsWith(literal)) {
      Success(literal, input.substring(literal.length))
    } else {
      Failure
    }
  }

  // --- string を s と短く呼ぶための別名メソッド ---
  /**
   * string parser
   * @param literal 文字列
   * @return
   */
  def s(literal: String): Parser[String] = string(literal)

  // --- 指定した文字のいずれかをパースするパーサ oneOf  ---
  def oneOf(chars: Seq[Char]): Parser[String] = input => {
    // 文字の長さが0でない、かつ、渡された文字のどれかにinputの最初の文字が該当すれば成功
    if (input.length != 0 &&
      chars.contains(input.head)) {
      Success(input.head.toString, input.tail)
    } else {
      Failure
    }
  }

  // --- パーサ同士を「選択」で合成するコンビネーター ---
  // 第一引数のパーサを試し、成功したらその結果を返す。失敗した場合は第二引数のパーサを試す
  // def 関数名[型パラメータ](引数名: 引数型, 引数名: 引数型): 返り値の型 = 本体
  def select[T, U >: T](left: => Parser[T], right: => Parser[U]): Parser[U] = input => {
    left(input) match {
      case success@Success(_, _) => success // 成功ならその結果を返す
      case Failure => right(input) // 失敗したらrightを試す
    }
  }

  // --- 逐次合成をするコンビネーター combine  ---
  // 渡された2つのパーサを左側から1つずつ適用、最終的に両方成功したら、両方の結果をタプルとして返す
  def combine[T, U](left: Parser[T], right: Parser[U]): Parser[(T, U)] = input => {
    left(input) match {
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

  // --- パース成功時の値の型をStringからBooleanに変換する ---
  def map[T, U](parser: Parser[T], function: T => U): Parser[U] = input => {
    parser(input) match {
      case Success(value, next) => Success(function(value), next) // 成功時のvalueだけ変換
      case Failure => Failure
    }
  }

}