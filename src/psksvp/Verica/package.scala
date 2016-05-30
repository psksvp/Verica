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
  implicit def string2Expression(src:String):Expression=Parser.parseExpression(src)
  implicit def string2Function(src:String):Function=Parser.parseFunction(src)
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
    case Length(v)        => List(Variable(s"lengthOf_${v.name}"))
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
        case Variable(name, _, _) if name == v.name => withExp
        case Binary(op, l, r)                       => Binary(op, substitutionOf(l), substitutionOf(r))
        case Unary(op, exp)                         => Unary(op, substitutionOf(exp))
        case _                                      => e
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
  def strongestPostCondition(assignment: Assignment, q:Predicate):Expression=
  {
    val vP = Variable(assignment.variable.name + "P",
                      assignment.variable.index,
                      assignment.variable.kind)
    val eq = equal(assignment.variable, substituteVariable(assignment.variable, assignment.expr, vP))
    QE.solve(Exists(vP :: Nil),
             SuchThat(and(eq, substituteVariable(assignment.variable, inPredicate = q, withExp = vP))))
  }

  def strongestPostCondition(stmt:Statement, q:Predicate):Formular =
  {
    import psksvp.Verica.PredicateAbstractionForSoftwareVerification._
    norm(q, stmt)
  }
  /**
    * "Background reading on Hoare Logic by Mike Gordon"
    * "page 73"
    *
    * @param stmt
    * @param q
    * @return
    */
  def awp(stmt:Statement, q:Formular):Formular = stmt match
  {
    case Assignment(v, e)      => substituteVariable(v, inPredicate = q, withExp = e)
    case If(s, c1, c2)         => val c1Path = and(s, awp(c1, q))
                                  val c2Path = and(not(s), awp(c2, q))
                                  or(c1Path, c2Path)
    case While(_, r, _, _)     => r
    case Sequence(s1)          => awp(s1, q)
    case Sequence(s1, rest@_*) => awp(s1, awp(Sequence(rest:_*), q))
    //case _                     => True()
  }

  /**
    * "Background reading on Hoare Logic by Mike Gordon"
    * "page 73"
    * @param stmt
    * @param q
    * @return
    */
  def wvc(stmt:Statement, q:Formular):Set[Formular] = stmt match
  {
    case Assignment(_, _)      => Set()
    case Sequence(s1)          => wvc(s1, q)
    case Sequence(c1, rest@_*) => wvc(c1, awp(Sequence(rest:_*), q)) union wvc(Sequence(rest:_*), q)
    case If(s, c1, c2)         => wvc(c1, q) union wvc(c2, q)
    case While(_, r, s, c)     => Set[Expression](implies(and(r, not(s)), q),
                                                  implies(and(r, s), awp(c, r))) union wvc(c, r)
    case _                     => Set()
  }

  def postConditionOf(ls:List[Statement]):List[Expression] = ls match
  {
    case Nil       => Nil
    case h :: rest => h match
                      {
                        case Ensure(e) => e :: postConditionOf(rest)
                        case _         => postConditionOf(rest)
                      }
  }

  def postConditionOf(function:Function):Expression = function.body match
  {
    case s:Sequence => and(postConditionOf(s.stmts.toList))
    case Ensure(e)  => e
    case _          => sys.error(s"function ${function.name} has no postcondition")
  }


  def verify(module:Module):Boolean=true
}
