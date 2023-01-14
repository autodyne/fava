package lisp

import gram._

object LispPEGs extends PEGs {
	def sexp: PEG[S] = list / quot / real / name
	def list = "(" ~> (sexp.* ^ List) <~ ")"
	def real = "[0-9]+".r ^ (real => Real(BigDecimal(real)))
	def name = """[^'`,@\(\)\s]+""".r ^ (name => Name(name))
	def quot = "'" ~> sexp ^ (Seq(Name("quote"), _)) ^ List
}

abstract class S(val exp: String, eval: S => Env => S) {
	override def toString = exp
	def apply(env: Env): S = eval(this)(env)
	def apply(env: Env)(args: Seq[S]): S = apply(env).asInstanceOf[Form].app(args, env)
}

case class Name(name: String) extends S(name, v => env => env.apply(v))
case class Real(real: BigDecimal) extends S(real.toString, v => _ => v)
case class Bool(bool: Boolean) extends S(bool.toString, v => _ => v)

case class List(list: Seq[S]) extends S(list.mkString("(", " ", ")"), _ => list.head(_)(list.tail))

class Form(exp: String, val app: (Seq[S], Env) => S) extends S(exp, v => _ => v)
class Lambda(p: List, v: S, e: Env) extends Form(s"(lambda $p $v)", (a, s) => v(Env(Some(e), p, s(a))))
class Syntax(p: List, v: S, e: Env) extends Form(s"(syntax $p $v)", (a, s) => v(Env(Some(e), p, a))(s))

case class Env(out: Option[Env], params: List, args: Seq[S]) {
	val map = params.list.zip(args).to(collection.mutable.Map)
	def apply(name: S): S = {
		if(map.isDefinedAt(name)) map(name)
		else if(out.nonEmpty) out.get(name)
		else sys.error(s"$name undeclared")
	}
	def apply(args: Seq[S]): Seq[S] = args.map(_.apply(this))
}

object LambdaForm extends Form("lambda", (a, s) => new Lambda(a.head.asInstanceOf[List], a(1), s))
object SyntaxForm extends Form("syntax", (a, s) => new Syntax(a.head.asInstanceOf[List], a(1), s))

class Forms(out: Option[Env]) extends Env(out, List(Seq()), Seq()) {
	def update(name: String, body: (Seq[S], Env) => S) = map(Name(name)) = Form(name, body)
}

class Prelude {
	val prelude = getClass.getResourceAsStream("/lisp.lisp")
	for(line <- io.Source.fromInputStream(prelude).getLines()) {
		LispPEGs.sexp(line).map(_.m(Root))
	}
	prelude.close
}

object Repl extends Prelude {
	val jline = new scala.tools.jline.console.ConsoleReader
	jline.setExpandEvents(false)
	jline.setPrompt(s"${Console.BLUE}lisp$$ ${Console.RESET}")
	def main(args: Array[String]) = while(true) try {
		val sexp = LispPEGs.sexp(jline.readLine)
		if(sexp.isDefined) println(sexp.get.m(Root).exp)
		else println(s"${Console.RED}compilation failure")
	} catch {
		case ex: Exception => println(s"${Console.RED} ${ex}")
	}
}

object Root extends Forms(None) {
	def put(scope: Env, name: Name, value: S): S = {
		scope.map(name) = value
		value
	}
	def exit: S = {
		System.exit(0)
		Name("exit")
	}
	this("quote")  = (args, scope) => args.head
	this("list")   = (args, scope) => List(args.map(_(scope)))
	this("car")    = (args, scope) => args.head(scope).asInstanceOf[List].list.head
	this("cdr")    = (args, scope) => List(args.head(scope).asInstanceOf[List].list.tail)
	this("lambda") = (args, scope) => new Lambda(args.head.asInstanceOf[List], args(1), scope)
	this("syntax") = (args, scope) => new Syntax(args.head.asInstanceOf[List], args(1), scope)
	this("set")    = (args, scope) => put(scope, args.head(scope).asInstanceOf[Name], args(1)(scope))
	this("+")      = (args, scope) => Real(args.map(_(scope).asInstanceOf[Real].real).reduce(_ + _))
	this("-")      = (args, scope) => Real(args.map(_(scope).asInstanceOf[Real].real).reduce(_ - _))
	this("*")      = (args, scope) => Real(args.map(_(scope).asInstanceOf[Real].real).reduce(_ * _))
	this("/")      = (args, scope) => Real(args.map(_(scope).asInstanceOf[Real].real).reduce(_ / _))
	this("eq")     = (args, scope) => Bool(args(0)(scope) == args(1)(scope))
	this("if")     = (args, scope) => args(if(args(0)(scope).asInstanceOf[Bool].bool) 1 else 2)(scope)
	this("exit")   = (args, scope) => exit
}
