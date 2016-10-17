package psksvp.Verica.Z3




/**
  * Created by psksvp on 17/10/16.
  */
object Interpolant
{
  import psksvp.Verica.Lang._
  import psksvp.Verica.Lang.Parser
  def compute(exprs:List[Expression]):List[Expression]=
  {
    val vars = for(e <- exprs) yield s"${makeVariables(e)}\n"
    val decl = vars.distinct.reduce(_ + _)
    var exprls = pythonize(exprs)
    val code =
      s"""
         |from z3 import *
         |$decl
         |print(sequence_interpolant([$exprls]))
      """.stripMargin.trim
    val ret = psksvp.evalPython(code)
    val pyExpr = Parser.parsePyZ3ListOutput(s"[$ret]")
    pyExpr
  }

  def checkTrace(S:List[Expression]):Satisfiable.CheckResult =
  {
    import psksvp.Verica._
    def pair(In:Expression,
             Sn:Expression,
             Inext:Expression):Expression=implies(and(In, Sn), Inext)

    val I = True() :: compute(S) ::: List(False())
    val e:Expression = (for(n <- (1 until S.length)) yield pair(I(n), S(n), I(n + 1))).reduce(and(_, _))

    Satisfiable.check(e)
  }
}
