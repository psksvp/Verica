package psksvp

/**
  * Created by psksvp on 11/04/2016.
  */
package object Verica
{
  import psksvp.Verica.Lang._

  type Predicate = Expression

  def and(left:Expression,
          right:Expression) = Binary(And(), left, right)

  def or(left:Expression,
         right:Expression) = Binary(Or(), left, right)

  def equal(left:Expression,
            right:Expression) = Binary(Equal(), left, right)

  def not(expr:Expression) = Unary(Negation(), expr)

  def targets(stmt:Statement):List[Variable] = stmt match
  {
    case Assignment(variable, _)  => List(variable)
    //http://www.scala-lang.org/old/node/2758
    //http://stackoverflow.com/questions/31064753/how-pass-scala-array-into-scala-vararg-method
    case Sequence(stm, rest @ _*) => targets(stm) ::: targets(Sequence(rest: _*))
    case _                        => Nil

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

  def desugar(w:While):Sequence = w match
  {
    case While(p, i, e, b) =>
      Sequence(Assert(i),
                havoc(targets(b)),
                Assume(i),
                Choice(Sequence(Assume(e), b, Assert(i), Assume(False())),
                        Assume(not(e))))
  }

  /**
    * subtitute variable v in ExpQ with expE
    *
    * @param v
    * @param expE
    * @param expQ
    * @return
    */
  def subtitude(v:Variable, expE:Expression, expQ:Expression):Expression =
  {
    expQ match
    {
      case Variable(n) if n == v.name => expE
      case Unary(op, e)               => Unary(op, subtitude(v, expE, e))
      case Binary(op, le, re)         => Binary(op,
                                                subtitude(v, expE, le),
                                                subtitude(v, expE, re))
      case _                          => expQ
    }
  }

  def norm(q:Predicate, s:Statement):Predicate = s match
  {
    case Assignment(v, e)        => and(True(), substituteVariable(v, inPredicate = q, withExp = e))
    case Assert(p)               => and(q, p)
    case Assume(p)               => and(q, p)
    case Choice(a, b)            => or(norm(q, a), norm(q, b))
    case Sequence(a)             => norm(q, a)
    case Sequence(a, b)          => norm(norm(q, a), b)
    case Sequence(a, b, rest@_*) => norm(norm(q, Sequence(a, b)), Sequence(rest:_*))
    case sw:While                => norm(q, desugar(sw))
  }

  def traverse(c:Statement, s:Statement):Statement = s match
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

  def infer(c:Statement, s:Statement):(Expression, Statement)= s match
  {
    case While(p, i, e, b) =>
      val h = havoc(targets(b))
      val r = alpha(norm(True(), c), p)
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

  //dummy for now
  def alpha(expr:Expression, pred:Predicates):Array[Boolean]=
  {
    println(s"$expr for predicate $pred")
    Array(true, false)
  }

  //dummy for now
  def gamma(absDom:Array[Boolean]):Expression=
  {
    False()
  }


  def substituteVariable(v:Variable, inPredicate:Predicate, withExp:Expression):Predicate=
  {
    def substitutionOf(e:Expression):Expression=
    {
      e match
      {
        case Variable(name) if name == v.name => withExp
        case Binary(op, l, r)                 => Binary(op, substitutionOf(l), substitutionOf(r))
        case Unary(op, exp)                   => Unary(op, substitutionOf(exp))
        case _                                => e
      }
    }

    inPredicate match
    {
      case Binary(op, l, r) => Binary(op, substitutionOf(l), substitutionOf(r))
      case Unary(op, e)     => Unary(op, substitutionOf(e))
      case _                => inPredicate
    }

  }

  def makePredicate(src:String):Predicate=Parser.parsePreidcate(src)

  def wp(stmt:Statement, q:Predicate):Predicate = stmt match
  {
    case Assignment(v, e)          => substituteVariable(v, inPredicate = q, withExp = e)
    case If(t, a, b)               => or(and(t, wp(a, q)), and(not(t), wp(b, q)))
    case Sequence(s1)              => wp(s1, q)
    case Sequence(s1, rest@_*)     => wp(s1, wp(Sequence(rest:_*), q))
  }
}
