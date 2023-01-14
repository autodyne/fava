package math

import gram._
import scala.language.implicitConversions

object ArithStackMachine extends collection.mutable.Stack[Int]() {
	def apply(program: String): Int = program.split(" +").map {
		case "+" => push(((a: Int, b: Int) => b + a)(pop(), pop()))
		case "-" => push(((a: Int, b: Int) => b - a)(pop(), pop()))
		case "*" => push(((a: Int, b: Int) => b * a)(pop(), pop()))
		case "/" => push(((a: Int, b: Int) => b / a)(pop(), pop()))
		case num => this.push(num.toInt)
	}.lastOption.map(_ => pop()).last
}

object ArithPEGs extends PEGs {
	def add: PEG[String] = new Fold(mul, ("+" / "-").^(op => (a, b) => s"$a $b $op"))
	def mul: PEG[String] = new Fold(num, ("*" / "/").^(op => (a, b) => s"$a $b $op"))
	def num: PEG[String] = "[0-9]+".r / ("(" ~> add <~ ")")
	def apply(e: String) = +ArithStackMachine(add(e).get.m)
}

object Repl {
	val jline = new scala.tools.jline.console.ConsoleReader
	jline.setExpandEvents(false)
	jline.setPrompt(s"${Console.BLUE}math$$ ${Console.RESET}")
	def main(args: Array[String]) = while(true) {
		val m = ArithPEGs.add(jline.readLine)
		if(m.isDefined) println(ArithStackMachine(m.get.m))
		else println(s"${Console.RED}illegal syntax found")
	}
}
