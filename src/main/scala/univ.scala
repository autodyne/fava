package univ

class UTM[V](data1: Seq[V], data2: Seq[V], b1: V, b2: V, mL: V, mR: V, var s1: V, var s2: Int = 0) {
	val tape1 = data1.zipWithIndex.map(_.swap).to(collection.mutable.SortedMap)
	val tape2 = data2.zipWithIndex.map(_.swap).to(collection.mutable.SortedMap)
	var hd1, hd2 = 0
	def r1 = tape1.getOrElse(hd1, b1)
	def r2 = tape2.getOrElse(hd2, b2)
	def apply(sop: V) = Iterator.continually(s2 match {
		case 0 if r2 == s1 => (s1 = s1, s2 = 1, tape1(hd1) = r1, hd1 += 0, hd2 += 1)
		case 0 if r2 != s1 => (s1 = s1, s2 = 0, tape1(hd1) = r1, hd1 += 0, hd2 += 5)
		case 1 if r2 == r1 => (s1 = s1, s2 = 2, tape1(hd1) = r1, hd1 += 0, hd2 += 1)
		case 1 if r2 != r1 => (s1 = s1, s2 = 0, tape1(hd1) = r1, hd1 += 0, hd2 += 4)
		case 2 if r2 != b2 => (s1 = r2, s2 = 3, tape1(hd1) = r1, hd1 += 0, hd2 += 1)
		case 3 if r2 != b2 => (s1 = s1, s2 = 4, tape1(hd1) = r2, hd1 += 0, hd2 += 1)
		case 4 if r2 == b1 => (s1 = s1, s2 = 5, tape1(hd1) = r1, hd1 += 0, hd2 += 1)
		case 4 if r2 == mL => (s1 = s1, s2 = 5, tape1(hd1) = r1, hd1 -= 1, hd2 += 1)
		case 4 if r2 == mR => (s1 = s1, s2 = 5, tape1(hd1) = r1, hd1 += 1, hd2 += 1)
		case 5 if r2 == b2 => (s1 = s1, s2 = 0, tape1(hd1) = r1, hd1 += 0, hd2 += 1)
		case 5 if r2 != b2 => (s1 = s1, s2 = 5, tape1(hd1) = r1, hd1 += 0, hd2 -= 1)
	}).takeWhile(t => s1 != sop || s2 != 0).map(t => tape1.values.mkString)
}
case class CUTM(data1: String, data2: String) extends UTM(data1, data2, ' ', '*', 'L', 'R', 'I')

object Repl {
	val jline = new scala.tools.jline.console.ConsoleReader
	jline.setExpandEvents(false)
	jline.setPrompt(s"${Console.BLUE}univ$$ ${Console.RESET}")
	val data2 = "I0a0RI1a1Ra0a0Ra1a1Ra b Lb0c1Lb1b0Lb F1 c0c0Lc1c1Lc F R"
	def main(args: Array[String]) = while(true) CUTM(jline.readLine, data2)('F').foreach(println)
}
