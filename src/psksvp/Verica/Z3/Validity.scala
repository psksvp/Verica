package psksvp.Verica.Z3


/**
  * Created by psksvp on 10/05/2016.
  */
object Validity extends com.typesafe.scalalogging.LazyLogging
{
  case class Valid()
  case class Invalid()

  import psksvp.Verica.Lang._

  def check(expr:Expression, assumptions:List[Expression] = Nil):Expression =
  {
    val pyExpr = pythonize(expr)
    val vars = makeIntVariables(expr)
    val assumptionCode = makeAssumptions("sOlVer", assumptions)
    val code =
      s"""
        |from z3 import *
        |$vars
        |sOlVer = Solver()
        |$assumptionCode
        |sOlVer.add(Not($pyExpr))
        |print(sOlVer.check())
      """.stripMargin.trim
    logger.trace(s"Validity.check($expr)")
    logger.trace(code)
    if("unsat" == psksvp.evalPython(code))
      True()
    else
      False()
  }
}
