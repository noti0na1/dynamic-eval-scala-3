//> using scala 3.10.0-RC1-bin-SNAPSHOT
//> using dep org.scala-lang:scala3-compiler_3:3.10.0-RC1-bin-SNAPSHOT
//> using dep org.scala-lang:scala3-repl_3:3.10.0-RC1-bin-SNAPSHOT
//> using options -Xdynamic-eval

import dotty.tools.eval.Eval.eval

@main def test: Unit =
  eval("""println("Hello, world!")""")