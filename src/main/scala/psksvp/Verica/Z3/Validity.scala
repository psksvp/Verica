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
    val vars = makeVariables(expr)
    val solver = psksvp.gensym()
    val code =
      s"""
        |from z3 import *
        |$vars
        |$solver = Solver()
        |${makeAssumptions(solver, assumptions)}
        |$solver.add(Not($pyExpr))
        |print($solver.check())
      """.stripMargin.trim
    logger.trace(s"Validity.check($expr)")
    logger.trace(code)
    if("unsat" == psksvp.evalPython(code))
      True()
    else
      False()
  }
}
