package psksvp.Verica.Z3

/**
  * Created by psksvp on 2/08/2016.
  */
object Simplify
{
  import psksvp.Verica.Lang._

  def apply(exp:Expression):Expression =
  {
    val pyExpCode = pythonize(exp)
    val varDecl = makeVariables(exp)

    val code =
      s"""
        |from z3 import *
        |$varDecl
        |print simplify($pyExpCode)
      """.stripMargin

    val result = psksvp.evalPython(code)
    val expr = Parser.parsePyZ3ListOutput(s"[[$result]]")
    expr.head
  }
}
