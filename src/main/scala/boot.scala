object Main {
	def main(args: Array[String]) = {
		args.headOption.getOrElse("--fava") match {
			case "--fava" => fava.Repl.main(args)
			case "--lisp" => lisp.Repl.main(args)
			case "--math" => math.Repl.main(args)
			case "--regl" => regl.Repl.main(args)
			case "--univ" => univ.Repl.main(args)
			case "--wire" => wire.Repl.main(args)
			case _ => println("incorrect option")
		}
	}
}
