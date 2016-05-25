package psksvp.Verica.Z3

/**
  * Created by psksvp on 18/05/2016.
  */
object Satisfiable
{
  case class Sat()
  case class Unsat()

  import psksvp.Verica.Lang._

  def check(expr:Expression):Expression =
  {
    val pyExpr = pythonize(expr)
    val vars = makeIntVariables(expr)
    val code =
      s"""
         |from z3 import *
         |$vars
         |sOlVer = Solver()
         |sOlVer.add($pyExpr))
         |print(sOlVer.check())
      """.stripMargin.trim
    if("unsat" == psksvp.evalPython(code))
      False()
    else
      True()
  }
}
