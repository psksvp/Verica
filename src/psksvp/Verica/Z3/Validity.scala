package psksvp.Verica.Z3

import psksvp.Verica._

/**
  * Created by psksvp on 10/05/2016.
  */
object Validity
{
  import psksvp.Verica.Lang._

  def check(expr:Expression):Expression =
  {
    val pyExpr = Z3.pythonize(expr)
    val vars = makeIntVariables(expr)
    val code =
      s"""
        |from z3 import *
        |$vars
        |s = Solver()
        |s.add(Not($pyExpr))
        |print(s.check())
      """.stripMargin.trim
    if("unsat" == psksvp.evalPython(code))
      True()
    else
      False()
  }
}
