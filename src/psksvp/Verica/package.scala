package psksvp

import psksvp.Verica.Z3.{Exists, QE, SuchThat}

/**
  * Created by psksvp on 11/04/2016.
  */
package object Verica
{
  import psksvp.Verica.Lang._

  type Predicate = Expression
  type Formular  = Expression

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
  def and(exprs:Expression*):Expression = and(exprs.toList)
  def and(exprs:List[Expression]):Expression = exprs match
  {
    case Nil          => sys.error("and(exprs:List[Expression]) is called with empty list")
    case expr :: Nil  => expr
    case expr :: rest => and(expr, and(rest))

  }

  def or(exprs:Expression*):Expression = or(exprs.toList)
  def or(exprs:List[Expression]):Expression = exprs match
  {
    case Nil          => sys.error("or(exprs:List[Expression]) is called with empty list")
    case expr :: Nil  => expr
    case expr :: rest => or(expr, or(rest))
  }

  def and(left:Expression,
          right:Expression) = Binary(And(), left, right)

  def or(left:Expression,
         right:Expression) = Binary(Or(), left, right)

  def equal(left:Expression,
            right:Expression) = Binary(Equal(), left, right)

  def implies(left:Expression,
             right:Expression) = Binary(Implies(), left, right)

  def not(expr:Expression) = Unary(Negation(), expr)

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

  /**
    *
    * @param stmt
    * @param q
    * @return
    */
  def weakestPrecondition(stmt:Statement, q:Predicate):Predicate = stmt match
  {
    case Assignment(v, e)          => substituteVariable(v, inPredicate = q, withExp = e)
    case If(t, a, b)               => or(and(t, weakestPrecondition(a, q)),
                                         and(not(t), weakestPrecondition(b, q)))
    case Sequence(s1)              => weakestPrecondition(s1, q)
    case Sequence(s1, rest@_*)     => weakestPrecondition(s1, weakestPrecondition(Sequence(rest:_*), q))
  }

  /**
    *
    * @param stmt
    * @param q
    * @return
    */
  def wp(stmt:Statement, q:Predicate) = weakestPrecondition(stmt, q)

  /**
    *
    * @param p
    * @param stm
    * @param q
    * @return
    */
  def vc(p:Expression, stm:Statement, q:Expression):Expression=stm match
  {
    case Assignment(v, e) => implies(p, substituteVariable(v, inPredicate = q, withExp = e))
    case If(s, c1, c2)    => and(vc(and(p, s), c1, q), vc(and(p, not(s)), c2,  q))
    case Sequence(s)      => vc(p, s, q)
    case s:Sequence       => implies(p, and(vc(p, Sequence(s.stmts.take(s.count - 1):_*), q),
                                            vc(p, s.stmts.last, q)))
  }

  /**
    *
    * @param assignment
    * @param q
    * @return
    */
  def strongestPostCondition(assignment: Assignment, q: Predicate):Expression=
  {
    val vP = Variable(assignment.variable.name + "P")
    val eq = equal(assignment.variable, substituteVariable(assignment.variable, assignment.expr, vP))
    QE.solve(Exists(vP :: Nil),
             SuchThat(and(eq, substituteVariable(assignment.variable, inPredicate = q, withExp = vP))))
  }
}
