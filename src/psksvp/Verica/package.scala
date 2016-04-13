package psksvp

/**
  * Created by psksvp on 11/04/2016.
  */
package object Verica
{
  import psksvp.Verica.Lang._

  def and(left:Expression,
          right:Expression) = Binary(And(), left, right)

  def or(left:Expression,
         right:Expression) = Binary(Or(), left, right)

  def not(expr:Expression) = Unary(Negation(), expr)

  def targets(stmt:Statement):List[Variable]=
  {
    stmt match
    {
      case Assignment(variable, _)  => List(variable)
      //http://www.scala-lang.org/old/node/2758
      //http://stackoverflow.com/questions/31064753/how-pass-scala-array-into-scala-vararg-method
      case Sequence(stm, rest @ _*) => targets(stm) ::: targets(Sequence(rest: _*))
      case _                        => Nil
    }
  }

  def havoc(variables:List[Variable]):Statement=
  {
    def makeAssignmentList(vars:List[Variable]):List[Assignment]=
    {
      vars match
      {
        case v :: rest => List(Assignment(v, Variable("y"+vars.size))) ::: makeAssignmentList(rest)
        case _         => Nil
      }
    }

    //http://stackoverflow.com/questions/31064753/how-pass-scala-array-into-scala-vararg-method
    Sequence(makeAssignmentList(variables): _*)
  }

  def desugar(w:While):Sequence =
  {
    w match
    {
      case While(p, i, e, b) =>
        Sequence(Assert(i),
                  havoc(targets(b)),
                  Assume(i),
                  Choice(Sequence(Assume(e), b, Assert(i), Assume(False())),
                          Assume(not(e))))
    }
  }

  def norm(q:Expression, s:Statement):Expression=
  {
    s match
    {
      case Assignment(_, _)        => and(q, True())
      case Assert(p)               => and(q, p)
      case Assume(p)               => and(q, p)
      case Choice(a, b)            => or(norm(q, a), norm(q, b))
      case Sequence(a)             => norm(q, a)
      case Sequence(a, b)          => norm(norm(q, a), b)
      case Sequence(a, b, rest@_*) => norm(norm(q, Sequence(a, b)), Sequence(rest:_*))
      case sw:While                => norm(q, desugar(sw))
    }
  }

  def traverse(c:Statement, s:Statement):Statement=
  {
    s match
    {
      case Assignment(_, _)          => s
      case Assert(_)                 => s
      case Assume(_)                 => s
      case Choice(a, b)              => Choice(traverse(c, a), traverse(c, b))
      case Sequence(a)               => traverse(c, a)
      case Sequence(a, b)            => val aP = traverse(c, a)
                                        val bP = traverse(Sequence(c, aP), b)
                                        Sequence(aP, bP)
      case Sequence(a, b, rest @ _*) => traverse(traverse(c, Sequence(a, b)), Sequence(rest: _*))
      case While(p, i, e, _)         => val (j, b) = infer(c, s)
                                        While(p, Invariant(and(i, j)), e, b)
    }
  }

  def infer(c:Statement, s:Statement):(Expression, Statement)=
  {
    s match
    {
      case While(p, i, e, b) =>
        val h = havoc(targets(b))
        val r = alpha(norm(True(), c))
        //while(true)
        //{
          val j  = gamma(r)
          val a  = Assume(and(and(e, i.expr), j))
          val bp = traverse(Sequence(c, h, a), b)
          val q  = norm(True(), Sequence(c, h, a, bp))

          (j, bp)
        //}

      case _ => sys.error("expect parm s to be a While")
    }
  }

  //dummy for now
  def alpha(expr:Expression):Array[Boolean]=
  {
    Array(true, false)
  }

  //dummy for now
  def gamma(absDom:Array[Boolean]):Expression=
  {
    False()
  }
}
