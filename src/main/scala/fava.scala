package fava

import gram.{Fold, PEG, PEGs, Sep}
import java.lang.{String => S}
import scala.language.implicitConversions
import scala.{Any => A, Int => I, Double => D, Boolean => B}

class Code(op: FaVM => Unit) {
	def apply(vm: FaVM) = (op(vm), vm.pc += 1)
}

case class Closure(from: Int, out: Env)
case class Promise(thunk: Closure, var cache: Any = null, var empty: Boolean = true)

case class Env(local: Seq[Any], out: Env = null) {
	def apply(depth: Int, index: Int): Any = depth match {
		case 0 => this.local(index)
		case d => out(d - 1, index)
	}
}

class FaVM(val codes: Seq[Code], var pc: Int = 0) {
	val call = new Stack[Env]
	val data = new Stack[Any]
	while(pc < codes.size) codes(pc)(this)
}

class Stack[E] extends collection.mutable.Stack[E] {
	def popN(n: Int) = Seq.fill(n)(pop).reverse
	def popAs[Type]: Type = pop.asInstanceOf[Type]
	def topAs[Type]: Type = top.asInstanceOf[Type]
	def env = (this :+ null).top.asInstanceOf[Env]
}

case class Push(v: Any) extends Code(vm => vm.data.push(v))

case class Load(nest: Int, id: Int) extends Code(vm => vm.data.push(vm.call.env(nest, id)))

class Arity(n: Int, f: Function[Seq[Any], Any]) extends Code(vm => vm.data.push(f(vm.data.popN(n))))
class Typed(val n: Int, val op: String, val t: Type, f: Function[Seq[Any], Any]) extends Arity(n, f)

class Op(op: Typed*)(val table: Map[(String, Type), Typed] = op.map(op => (op.op, op.t) -> op).toMap)

case object IPos extends Typed(1, "+", It, +_.head.asInstanceOf[I])
case object DPos extends Typed(1, "+", Dt, +_.head.asInstanceOf[D])

case object INeg extends Typed(1, "-", It, -_.head.asInstanceOf[I])
case object DNeg extends Typed(1, "-", Dt, -_.head.asInstanceOf[D])

case object BNot extends Typed(1, "!", Bt, !_.head.asInstanceOf[B])

object UnOp extends Op(IPos, DPos, INeg, DNeg, BNot)()

case object IAdd extends Typed(2, "+", It, v => v.head.asInstanceOf[I] + v.last.asInstanceOf[I])
case object DAdd extends Typed(2, "+", Dt, v => v.head.asInstanceOf[D] + v.last.asInstanceOf[D])
case object SAdd extends Typed(2, "+", St, v => v.head.asInstanceOf[S] + v.last.asInstanceOf[S])

case object ISub extends Typed(2, "-", It, v => v.head.asInstanceOf[I] - v.last.asInstanceOf[I])
case object DSub extends Typed(2, "-", Dt, v => v.head.asInstanceOf[D] - v.last.asInstanceOf[D])
case object SSub extends Typed(2, "-", St, v => v.head.asInstanceOf[S].replace(v.last.asInstanceOf[S], ""))

case object IMul extends Typed(2, "*", It, v => v.head.asInstanceOf[I] * v.last.asInstanceOf[I])
case object DMul extends Typed(2, "*", Dt, v => v.head.asInstanceOf[D] * v.last.asInstanceOf[D])

case object IDiv extends Typed(2, "/", It, v => v.head.asInstanceOf[I] / v.last.asInstanceOf[I])
case object DDiv extends Typed(2, "/", Dt, v => v.head.asInstanceOf[D] / v.last.asInstanceOf[D])

case object IMod extends Typed(2, "%", It, v => v.head.asInstanceOf[I] % v.last.asInstanceOf[I])
case object DMod extends Typed(2, "%", Dt, v => v.head.asInstanceOf[D] % v.last.asInstanceOf[D])

case object IGt extends Typed(2, ">", It, v => v.head.asInstanceOf[I] > v.last.asInstanceOf[I])
case object DGt extends Typed(2, ">", Dt, v => v.head.asInstanceOf[D] > v.last.asInstanceOf[D])

case object ILt extends Typed(2, "<", It, v => v.head.asInstanceOf[I] < v.last.asInstanceOf[I])
case object DLt extends Typed(2, "<", Dt, v => v.head.asInstanceOf[D] < v.last.asInstanceOf[D])

case object IGe extends Typed(2, ">=", It, v => v.head.asInstanceOf[I] >= v.last.asInstanceOf[I])
case object DGe extends Typed(2, ">=", Dt, v => v.head.asInstanceOf[D] >= v.last.asInstanceOf[D])

case object ILe extends Typed(2, "<=", It, v => v.head.asInstanceOf[I] <= v.last.asInstanceOf[I])
case object DLe extends Typed(2, "<=", Dt, v => v.head.asInstanceOf[D] <= v.last.asInstanceOf[D])

case object IEq extends Typed(2, "==", It, v => v.head.asInstanceOf[I] == v.last.asInstanceOf[I])
case object DEq extends Typed(2, "==", Dt, v => v.head.asInstanceOf[D] == v.last.asInstanceOf[D])
case object SEq extends Typed(2, "==", St, v => v.head.asInstanceOf[S] == v.last.asInstanceOf[S])
case object BEq extends Typed(2, "==", Bt, v => v.head.asInstanceOf[B] == v.last.asInstanceOf[B])

case object INe extends Typed(2, "!=", It, v => v.head.asInstanceOf[I] != v.last.asInstanceOf[I])
case object DNe extends Typed(2, "!=", Dt, v => v.head.asInstanceOf[D] != v.last.asInstanceOf[D])
case object SNe extends Typed(2, "!=", St, v => v.head.asInstanceOf[S] != v.last.asInstanceOf[S])
case object BNe extends Typed(2, "!=", Bt, v => v.head.asInstanceOf[B] != v.last.asInstanceOf[B])

case object IAnd extends Typed(2, "&", It, v => v.head.asInstanceOf[I] & v.last.asInstanceOf[I])
case object BAnd extends Typed(2, "&", Bt, v => v.head.asInstanceOf[B] & v.last.asInstanceOf[B])

case object IOr extends Typed(2, "|", It, v => v.head.asInstanceOf[I] | v.last.asInstanceOf[I])
case object BOr extends Typed(2, "|", Bt, v => v.head.asInstanceOf[B] | v.last.asInstanceOf[B])

object AddOp extends Op(IAdd, DAdd, SAdd, ISub, DSub, SSub)()
object MulOp extends Op(IMul, DMul, IDiv, DDiv, IMod, DMod)()
object RelOp extends Op(IGt, DGt, ILt, DLt, IGe, DGe, ILe, DLe)()
object EqlOp extends Op(IEq, DEq, SEq, BEq, INe, DNe, SNe, BNe)()
object LogOp extends Op(IAnd, BAnd, IOr, BOr)()

class Jump(op: FaVM => Option[Int]) extends Code(vm => op(vm).foreach(to => vm.pc = to - 1))

case class Skip(plus: Int) extends Jump(vm => Some(vm.pc + plus))
case class Skin(plus: Int) extends Jump(vm => Option.when(!vm.data.popAs[B])(vm.pc + plus))

case class Def(size: Int) extends Jump(vm => Some {
	vm.data.push(Closure(vm.pc + 1, vm.call.env))
	vm.pc + size
})

case object Ret extends Jump(vm => Some {
	vm.call.remove(0).asInstanceOf[Env]
	vm.data.remove(1).asInstanceOf[Int]
})

case class Call(argc: Int) extends Jump(vm => Some {
	val args = vm.data.popN(argc)
	val func = vm.data.popAs[Closure]
	vm.call.push(Env(args, func.out))
	vm.data.push(vm.pc + 1)
	func.from
})

case object Arg extends Code(vm => vm.data.push(Promise(vm.data.popAs[Closure])))
case object Get extends Code(vm => vm.data.push(vm.data.popAs[Promise].cache))
case object Nil extends Code(vm => vm.data.push(vm.data.topAs[Promise].empty))
case object Ref extends Code(vm => vm.data.push(vm.data.topAs[Promise].thunk))
case object Set extends Code(vm => vm.data.popAs[Promise].cache = vm.data.pop)
case object Fix extends Code(vm => vm.data.topAs[Promise].empty = false)

trait AST {
	def res(implicit env: Seq[DefST]): Type
	def gen(implicit env: Seq[DefST]): Seq[Code]
	def acc(unify: => Unit)(v: Type) = util.Try(unify).map(_ => v).get
}

case class LitST(value: Any) extends AST {
	def res(implicit env: Seq[DefST]) = Atom(value.getClass)
	def gen(implicit env: Seq[DefST]) = Seq(Push(value))
}

case class StrST(string: String) extends AST {
	def res(implicit env: Seq[DefST]) = Atom(classOf[String])
	def gen(implicit env: Seq[DefST]) = LitST(StringContext.processEscapes(string)).gen
}

case class UnST(op: String, expr: AST) extends AST {
	def res(implicit env: Seq[DefST]) = acc(Form(v).unify(Form(expr.res)))(v)
	def gen(implicit env: Seq[DefST]) = expr.gen :+ UnOp.table(op, v.prune)
	val v = new Link
}

case class AddST(op: String, e1: AST, e2: AST) extends AST {
	def res(implicit env: Seq[DefST]) = acc(Form(v, v).unify(Form(e1.res, e2.res)))(v)
	def gen(implicit env: Seq[DefST]) = e1.gen ++ e2.gen :+ AddOp.table(op, v.prune)
	val v = new Link
}

case class MulST(op: String, e1: AST, e2: AST) extends AST {
	def res(implicit env: Seq[DefST]) = acc(Form(v, v).unify(Form(e1.res, e2.res)))(v)
	def gen(implicit env: Seq[DefST]) = e1.gen ++ e2.gen :+ MulOp.table(op, v.prune)
	val v = new Link
}

case class RelST(op: String, e1: AST, e2: AST) extends AST {
	def res(implicit env: Seq[DefST]) = acc(Form(v, v).unify(Form(e1.res, e2.res)))(Bt)
	def gen(implicit env: Seq[DefST]) = e1.gen ++ e2.gen :+ RelOp.table(op, v.prune)
	val v = new Link
}

case class EqlST(op: String, e1: AST, e2: AST) extends AST {
	def res(implicit env: Seq[DefST]) = acc(Form(v, v).unify(Form(e1.res, e2.res)))(Bt)
	def gen(implicit env: Seq[DefST]) = e1.gen ++ e2.gen :+ EqlOp.table(op, v.prune)
	val v = new Link
}

case class LogST(op: String, e1: AST, e2: AST) extends AST {
	def res(implicit env: Seq[DefST]) = acc(Form(v, v).unify(Form(e1.res, e2.res)))(v)
	def gen(implicit env: Seq[DefST]) = e1.gen ++ e2.gen :+ LogOp.table(op, v.prune)
	val v = new Link
}

case class IfST(c: AST, e: (AST, AST)) extends AST {
	def pos(pos: Seq[Code]) = (Skin(2 + pos.size) +: pos)
	def neg(neg: Seq[Code]) = (Skip(1 + neg.size) +: neg)
	def res(implicit env: Seq[DefST]) = acc(Form(Bt, v, v).unify(Form(c.res, e._1.res, e._2.res)))(v)
	def gen(implicit env: Seq[DefST]) = c.gen ++ pos(e._1.gen) ++ neg(e._2.gen)
	val v = new Link
}

case class DefST(params: Seq[String], value: AST) extends AST {
	val args = params.map(_ -> new Link).toMap
	def get(name: String, depth: Int) = Load(depth, params.indexOf(name)) -> args(name)
	def res(implicit env: Seq[DefST]) = Form(params.map(args) :+ value.res(env :+ this) :_*)
	def gen(implicit env: Seq[DefST]) = tag(value.gen(env :+ this))
	def tag(codes: Seq[Code]) = Def(codes.size + 2) +: codes :+ Ret
}

case class StIdST(val name: String) extends AST {
	def resolve(env: Seq[DefST], nest: Int = 0): (Load, Link) = {
		if(env.last.params.contains(name)) env.last.get(name, nest)
		else if(env.size >= 2) resolve(env.init, nest + 1)
		else sys.error(s"parameter $name is not declared")
	}
	def res(implicit env: Seq[DefST]) = resolve(env)._2.prune
	def gen(implicit env: Seq[DefST]) = Seq(resolve(env)._1)
}

case class LzIdST(val name: StIdST) extends AST {
	def res(implicit env: Seq[DefST]) = name.res
	def gen(implicit env: Seq[DefST]) = (name.gen ++ head ++ name.gen ++ tail)
	val (head, tail) = List(Nil, Skin(6), Ref, Call(0)) -> List(Fix, Set, Get)
}

case class LzArgST(body: AST) extends AST {
	def res(implicit env: Seq[DefST]) = body.res
	def gen(implicit env: Seq[DefST]) = DefST(Seq(), body).gen :+ Arg
}

case class CallST(f: AST, args: Seq[AST]) extends AST {
	def res(implicit env: Seq[DefST]) = acc(Form(args.map(_.res) :+ v :_*).prune.unify(f.res))(v)
	def gen(implicit env: Seq[DefST]) = f.gen ++ args.map(_.gen).flatten :+ Call(args.size)
	val v = new Link
}

case class ExitST(exit: String) extends AST {
	def res(implicit env: Seq[DefST]) = null
	def gen(implicit env: Seq[DefST]) = null
	System.exit(0)
}

case class CompileST(expr: AST) extends AST {
	def res(implicit env: Seq[DefST]) = expr.res
	def gen(implicit env: Seq[DefST]) = Seq(Push(expr.gen.mkString(" ")))
}

trait Type {
	def prune = this
	def unify(t: Type): Unit
}

case class Atom(atom: Class[_]) extends Type {
	def unify(t: Type) = t.prune match {
		case t: Link => t.unify(this)
		case t: Type => require(this == t)
	}
}

case class Form(dom: Type*) extends Type {
	def unify(t: Type) = t.prune match {
		case t: Form => t.align(this)
		case t: Type => t.unify(this)
	}
	def align(t: Form) = {
		require(this.dom.size == t.dom.size)
		dom.zip(t.dom).map(_.prune.unify(_))
	}
}

class Link(var to: Option[Type] = None) extends Type {
	def unify(t: Type) = t.prune match {
		case t: Form => to = Some(Loop(t, this).prune)
		case t: Type => to = Option.when(this != t)(t)
	}
	override def prune = to.map(_.prune).getOrElse(this)
}

case class Loop(form: Form, link: Link) extends Type {
	def unify(t: Type) = t match {
		case t: Form => link.unify(t)
		case t: Type => require(this == t)
	}
	override def prune = if(form.dom.contains(link)) this else form
}

object Bt extends Atom(classOf[java.lang.Boolean])
object It extends Atom(classOf[java.lang.Integer])
object Dt extends Atom(classOf[java.lang.Double])
object St extends Atom(classOf[java.lang.String])

object FavaPEGs extends PEGs {
	def expr: PEG[AST] = (cond / or) <~ ("//" ~ ".*$".r).?
	def cond = (or <~ "?") ~ (expr ~ (":" ~> expr)) ^ IfST
	def or   = new Fold(and, "|" ^ (op => LogST(op, _, _)))
	def and  = new Fold(eql, "&" ^ (op => LogST(op, _, _)))
	def eql  = new Fold(rel, """(!|=)=""".r ^ (op => EqlST(op, _, _)))
	def rel  = new Fold(add, """[<>]=?""".r ^ (op => RelST(op, _, _)))
	def add  = new Fold(mul, """[\+\-]""".r ^ (op => AddST(op, _, _)))
	def mul  = new Fold(unr, """[\*/%]""".r ^ (op => MulST(op, _, _)))
	def unr  = ("+" / "-" / "!").* ~ call ^ ((o,e) => o.foldRight(e)(UnST))
	def call = fact ~ args.* ^ ((f,a) => a.foldLeft(f)(CallST))
	def args = "(" ~> new Sep(expr ^ LzArgST, ",") <~")"
	def fact = func / bool / text / real / int / name / ("(" ~> expr <~ ")")
	def func = pars ~ ("=>" ~> expr) ^ ((p,e) => DefST(p, e))
	def pars = "(" ~> new Sep(name, ",") <~ ")" ^ (_.map(_.name.name))
	def bool = ("true" / "false") ^ (_.toBoolean) ^ LitST
	def text = ("\"" ~> """([^"\\]|\\[\\'"bfnrt])*""".r <~ "\"") ^ StrST
	def int  = """\d+""".r ^ (_.toInt) ^ LitST
	def real = """(\d+\.\d*|\d*\.\d+)""".r ^ (_.toDouble) ^ LitST
	def name = """[@A-Z_a-z][@0-9A-Z_a-z]*""".r ^ StIdST ^ LzIdST
}

object FavaReplPEGs extends gram.PEGs {
	def full: PEG[AST] = pile / exit / expr
	def expr: PEG[AST] = FavaPEGs.expr
	def exit: PEG[AST] = "exit" ^ ExitST
	def pile: PEG[AST] = "compile" ~ "(" ~> expr <~ ")" ^ CompileST
}

object Repl {
	val jline = new scala.tools.jline.console.ConsoleReader
	jline.setExpandEvents(false)
	jline.setPrompt(s"${Console.BLUE}fava$$ ${Console.RESET}")
	def main(args: Array[String]) = while(true) try {
		val ast = FavaReplPEGs.full(jline.readLine)
		if(ast.isDefined) {
			val t = ast.get.m.res(Seq())
			val c = ast.get.m.gen(Seq())
			println(new FaVM(c).data.pop)
		} else println(s"${Console.RED}compilation failure: illegal syntax")
	} catch {
		case ex: Exception => println(s"${Console.RED} $ex")
	}
}
