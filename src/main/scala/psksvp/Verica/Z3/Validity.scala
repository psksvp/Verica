package psksvp.Verica.Z3


/**
  * Created by psksvp on 10/05/2016.
  */
object Validity extends com.typesafe.scalalogging.LazyLogging
{
  import psksvp.Verica.Lang._
  abstract class CheckResult
  case class Valid(expression: Expression) extends CheckResult
  case class Invalid(expression: Expression) extends CheckResult

  def check(expr: Expression,
            assumptions: List[Expression] = Nil): CheckResult =
  {
    import psksvp.Verica.not
    Satisfiable.check(not(expr)) match
    {
      case Satisfiable.Unsat(_) => Valid(expr)
      case Satisfiable.Sat(_) => Invalid(expr)
    }
  }
}
//    val pyExpr = pythonize(expr)
//    val vars = makeVariables(expr)
//    val solver = psksvp.gensym()
//    val code =
//      s"""
//        |from z3 import *
//        |$vars
//        |$solver = Solver()
//        |${makeAssumptions(solver, assumptions)}
//        |$solver.add(Not($pyExpr))
//        |print($solver.check())
//      """.stripMargin.trim
//    logger.trace(s"Validity.check($expr)")
//    logger.trace(code)
//    if("unsat" == psksvp.evalPython(code))
//      Valid(expr)
//    else
//      Invalid(expr)
//  }
//}
