package regl

class R[S](val test: Seq[S] => Option[Seq[S]])

case class One[S](r: S) extends R[S](Some(_).filter(_.head == r).map(_.tail))

case class Cat[S](l: R[S], r: R[S]) extends R[S](seq => l.test(seq).map(r.test).flatten)
case class Alt[S](l: R[S], r: R[S]) extends R[S](seq => l.test(seq).orElse(r.test(seq)))

case class Opt[S](r: R[S]) extends R[S](seq => r.test(seq).orElse(Some(seq)))
case class Rep[S](r: R[S]) extends R[S](seq => Cat(r, Opt(Rep(r))).test(seq))

object Repl {
	val ZLO = Cat(One('Z'), Cat(Alt(Rep(One('L')), One('G')), One('O')))
	val jline = new scala.tools.jline.console.ConsoleReader
	jline.setExpandEvents(false)
	jline.setPrompt(s"${Console.BLUE}$ZLO$$ ${Console.RESET}")
	def main(args: Array[String]) = while(true) println(ZLO.test(jline.readLine).isDefined)
}
