package jp.ed.nnn.parsercombinator

import scala.annotation.tailrec
import scala.io.Source

object ChatBotMain extends App {
  val text = Source.fromFile("./chatbot.txt").mkString
  val chatBot: ChatBot = ChatBotTextParser(text) match {
    case ChatBotTextParser.Success(result, _) => result
    case failure: ChatBotTextParser.NoSuccess =>  scala.sys.error(failure.toString)
  }
  println("chatBot: " + chatBot)
  println("ChatBot booted.")

  @tailrec
  def checkInput(): Unit = {
    val input = scala.io.StdIn.readLine(">> ")
    if(input.startsWith("exit")) System.exit(0)

    @tailrec
    def execute(input: String, commands: List[Command]): Unit = {
      if (commands.nonEmpty && !commands.head.exec(input)) {
        execute(input, commands.tail)
      }
    }
    execute(input, chatBot.commands)
    checkInput()
  }
  checkInput()
}