package gram

import scala.language.implicitConversions
import scala.util.matching.Regex

class PEG[+M](f: String => Option[Out[M]]) {
	def skip = Reg("""\s*""".r) ~> this <~ Reg("""\s*""".r)
	def / [R >: M](q: => PEG[R]): PEG[R] = new Alt(this, q)
	def ~ [R](q: => PEG[R]): PEG[(M, R)] = new Cat(this, q)
	def <~[R](q: => PEG[R]) = this ~ q ^ (_._1)
	def ~>[R](q: => PEG[R]) = this ~ q ^ (_._2)
	def ^ [T](f: M => T) = new Map(this, f)
	def * = new Rep(this)
	def ? = new Opt(this)
	def apply(in: String) = f(in)
}

case class Out[+M](m: M, in: String) {
	def tuple[R](o: Out[R]) = Out(m -> o.m, o.in)
	def apply[R](p: PEG[R]) = p(in).map(tuple(_))
	def toSome = Out(Some(m), in)
}

case class Str(p: String) extends PEG(s => Option.when(s.startsWith(p))(Out(p, s.substring(p.length))))
case class Reg(p: Regex) extends PEG(p.findPrefixMatchOf(_).map(m => Out(m.matched, m.after.toString)))

class Alt[L, R >: L](p: => PEG[L], q: => PEG[R]) extends PEG[R](s => p(s) orElse q(s))
class Cat[+L, +R](p: => PEG[L], q: => PEG[R]) extends PEG(p(_).map(_ apply q).flatten)

class Map[+S, +T](p: => PEG[S], f: S => T) extends PEG[T](p(_).map(t => Out(f(t.m), t.in)))
class Opt[+T](p: => PEG[T]) extends PEG(s => p(s).map(_.toSome).orElse(Some(Out(None, s))))

class And[+T](p: => PEG[T]) extends PEG(s => if(p(s).isDefined) Some(Out(None, s)) else None)
class Not[+T](p: => PEG[T]) extends PEG(s => if(p(s).isDefined) None else Some(Out(None, s)))

class Rep[+T](p: => PEG[T]) extends PEG(s => {
	def ca(a: Out[T]): Out[Seq[T]] = Out(a.m +: re(a.in).m, re(a.in).in)
	def re(s: String): Out[Seq[T]] = p(s).map(ca).getOrElse(Out(Nil, s))
	Some(re(s))
})

class Fold[T](p: => PEG[T], q: => PEG[(T, T) => T]) extends PEG({
	(p ~ (q ~ p).*)^(x => x._2.foldLeft(x._1)((l, r) => r._1(l, r._2)))
} apply(_))

class Sep[T](p: => PEG[T], q: => PEG[_]) extends Fold[Seq[T]](p.? ^ (_.toSeq), q^(_ => _ ++ _))

class PEGs {
	implicit def implicitText(p: String): PEG[String] = new Str(p).skip
	implicit def implicitRegex(p: Regex): PEG[String] = new Reg(p).skip
}
