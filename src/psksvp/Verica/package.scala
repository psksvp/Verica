package psksvp

/**
  * Created by psksvp on 11/04/2016.
  */
package object Verica
{
  import psksvp.Verica.Lang._

  type Predicate = Expression
  implicit def string2Statement(src:String):Statement=Parser.parseStatement(src)
  implicit def string2Assignment(src:String):Assignment=Parser.parseStatement(src).asInstanceOf[Assignment]
  implicit def string2Expression(src:String):Expression=Parser.parseExpression(src)

  implicit def string2ListOfVariable(src:String):Seq[Variable]=
  {
    var r:List[Variable] = Nil
    for(s <- src.split(","))
      r = r :+ Variable(s)

    r
  }

  def listOfVariablesIn(expression: Expression):List[Variable] = expression match
  {
    case v:Variable       => List(v)
    case Binary(op, l, r) => listOfVariablesIn(l) ::: listOfVariablesIn(r)
    case Unary(op, e)     => listOfVariablesIn(e)
    case _                => Nil
  }


  ////////////////////////////////////////////
  def and(left:Expression,
          right:Expression) = Binary(And(), left, right)

  def or(left:Expression,
         right:Expression) = Binary(Or(), left, right)

  def equal(left:Expression,
            right:Expression) = Binary(Equal(), left, right)

  def implies(left:Expression,
             right:Expression) = Binary(Implies(), left, right)

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


  def norm(q:Predicate, s:Statement):Predicate = s match
  {
    case a:Assignment            => strongestPostCondition(a, q)
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
    case Sequence(a, rest@_*)      => val aP = traverse(c, a)
                                      traverse(aP, Sequence(rest: _*))
    case Sequence(a, b)            => val aP = traverse(c, a)
                                      val bP = traverse(Sequence(c, aP), b)
                                      Sequence(aP, bP)
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


  /**
    * substitute variable v in inPredicate with expression withExp
    *
    * @param v
    * @param inPredicate
    * @param withExp
    * @return an expression
    */
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

  def weakestPrecondition(stmt:Statement, q:Predicate):Predicate = stmt match
  {
    case Assignment(v, e)          => substituteVariable(v, inPredicate = q, withExp = e)
    case If(t, a, b)               => or(and(t, weakestPrecondition(a, q)),
                                         and(not(t), weakestPrecondition(b, q)))
    case Sequence(s1)              => weakestPrecondition(s1, q)
    case Sequence(s1, rest@_*)     => weakestPrecondition(s1, weakestPrecondition(Sequence(rest:_*), q))
  }

  def wp(stmt:Statement, q:Predicate) = weakestPrecondition(stmt, q)


  def vc(p:Expression, stm:Statement, q:Expression):Expression=stm match
  {
    case Assignment(v, e) => implies(p, substituteVariable(v, inPredicate = q, withExp = e))
    case If(s, c1, c2)    => and(vc(and(p, s), c1, q), vc(and(p, not(s)), c2,  q))
    case Sequence(s)      => vc(p, s, q)
    case s:Sequence       => implies(p, and(vc(p, Sequence(s.stmts.take(s.count - 1):_*), q),
                                          vc(p, s.stmts.last, q)))
  }


  def strongestPostCondition(assignment: Assignment, q: Predicate):Expression=
  {
    import psksvp.Verica.QuantifierElimination._
    val vP = Variable(assignment.variable.name + "P")
    val eq = equal(assignment.variable, substituteVariable(assignment.variable, assignment.expr, vP))
    QE(Exists(vP :: Nil), SuchThat(and(eq, substituteVariable(assignment.variable, inPredicate = q, withExp = vP))))
  }
}
