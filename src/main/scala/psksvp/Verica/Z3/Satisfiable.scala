package psksvp.Verica.Z3

/**
  * Created by psksvp on 18/05/2016.
  */
object Satisfiable
{
  import psksvp.Verica.Lang._
  abstract class CheckResult
  case class Sat(expression:Expression) extends CheckResult
  case class Unsat(expression:Expression) extends CheckResult

  def check(expr:Expression,
            assumptions:List[Expression] = Nil):CheckResult =
  {
    val pyExpr = pythonize(expr)
    val vars = makeVariables(expr)
    val solver = psksvp.gensym()
    val assump = makeAssumptions(solver, assumptions)
    val code =
      s"""
         |from z3 import *
         |$vars
         |$solver = Solver()
         |$assump
         |$solver.add($pyExpr)
         |print($solver.check())
      """.stripMargin.trim
    val ret = psksvp.evalPython(code)
    //println(s"Satisfiable.check return string ===> $ret for $expr")
    if("unsat" == ret)
      Unsat(expr)
    else
      Sat(expr)
  }
}
