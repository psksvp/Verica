package psksvp

import psksvp.Verica.Z3.{Exists, QE, SuchThat, Validity}
/**
  * Created by psksvp on 11/04/2016.
  */
package object Verica extends com.typesafe.scalalogging.LazyLogging
{
  import psksvp.Verica.Lang._

  type Predicate = Expression
  type Formular  = Expression

  implicit def string2Predicates(src:String):Predicates=Parser.parsePredicates(src)
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
    case v:Variable                   => List(v)
    case Binary(op, l, r)             => listOfVariablesIn(l) ::: listOfVariablesIn(r)
    case Unary(op, e)                 => listOfVariablesIn(e)
    case Length(v)                    => List(Variable(s"lengthOf_${v.name}"))
    case UniversalQuantifier(vl, e)   => vl ::: listOfVariablesIn(e)
    case ExistentialQuantifier(vl, e) => vl ::: listOfVariablesIn(e)
    case _                            => Nil
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
    * @param inExp
    * @param withExp
    * @return an expression
    */
  def substituteVariable(v:Variable, inExp:Expression, withExp:Expression):Expression=
  {
    def substitutionOf(e:Expression):Expression=
    {
      e match
      {
        case Variable(name, _, _) if name == v.name => withExp
        case Binary(op, l, r)                       => Binary(op, substitutionOf(l),
                                                                  substitutionOf(r))
        case Unary(op, exp)                         => Unary(op, substitutionOf(exp))
        case _                                      => e
      }
    }

    inExp match
    {
      case Binary(op, l, r)             => Binary(op, substitutionOf(l),
                                                      substitutionOf(r))
      case Unary(op, e)                 => Unary(op, substitutionOf(e))
      case UniversalQuantifier(vl, e)   => UniversalQuantifier(vl, substitutionOf(e))
      case ExistentialQuantifier(vl, e) => ExistentialQuantifier(vl, substitutionOf(e))
      case _                            => inExp
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
    case Assignment(v, e)          => substituteVariable(v, inExp = q, withExp = e)
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
    case Assignment(v, e) => implies(p, substituteVariable(v, inExp = q, withExp = e))
    case If(s, c1, c2)    => and(vc(and(p, s), c1, q), vc(and(p, not(s)), c2,  q))
    case Sequence(s)      => vc(p, s, q)
    case s:Sequence       => implies(p, and(vc(p, Sequence(s.stmts.take(s.count - 1):_*), q),
                                            vc(p, s.stmts.last, q)))

    case While(_, i, e, s)=> and(implies(p, i),
                                 implies(and(i, not(e)), q),
                                 vc(and(i, e), s, i))
  }

  /**
    *
    * @param assignment
    * @param q
    * @return
    */
  def strongestPostCondition(assignment: Assignment, q:Predicate):Expression=
  {
    assignment match
    {
      case Assignment(v, e)            =>
        val vP = Variable(v.name + "P", v.index, v.kind)
        val eq = equal(v, substituteVariable(v, inExp = e, withExp = vP))
        QE.solve(Exists(vP :: Nil), SuchThat(and(eq, substituteVariable(v, inExp = q, withExp = vP))))
    }
  }

  /**
    *
    * @param stmt
    * @param q
    * @return
    */
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
    case Assignment(v, e)      => substituteVariable(v, inExp = q, withExp = e)
    case If(s, c1, c2)         => val c1Path = and(s, awp(c1, q))
                                  val c2Path = and(not(s), awp(c2, q))
                                  or(c1Path, c2Path)
    case While(_, r, _, _)     => r
    case Sequence(s1)          => awp(s1, q)
    case Sequence(s1, rest@_*) => awp(s1, awp(Sequence(rest:_*), q))
    case Assert(e)             => and(e, q)
    case Assume(e)             => implies(e, q)
    case _                     => True()
  }

  /**
    * "Background reading on Hoare Logic by Mike Gordon"
    * "page 73"
    *
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

  /**
    *
    * @param function
    * @return
    */
  def postConditionOf(function:Function):List[Expression] =
  {
    /**
      *
      * @param ls
      * @return
      */
    def lookForEnsure(ls:List[VerificationStatment]):List[Expression] = ls match
    {
      case Nil               => Nil
      case Ensure(e) :: rest => e :: lookForEnsure(rest)
      case _ :: rest         => lookForEnsure(rest)
    }

    lookForEnsure(function.verificationStatments)
  }

  /**
    *
    * @param function
    * @return
    */
  def assumptionOf(function:Function):List[Expression] =
  {
    def lookForAssume(ls:List[VerificationStatment]):List[Expression] = ls match
    {
      case Nil               => Nil
      case Assume(e) :: rest => e :: lookForAssume(rest)
      case _ ::rest          => lookForAssume(rest)
    }

    lookForAssume(function.verificationStatments)
  }

  /**
    *
    * @param function
    * @return
    */
  def verify(function:Function):(Expression, Boolean) =
  {
    val listOfpostCond = postConditionOf(function)
    if(Nil != listOfpostCond)
    {
      val listOfAssumption = assumptionOf(function)

      logger.trace(s"goint to verify function ${function.name} with post conds $listOfpostCond")
      var resultExp:Expression = True()
      var resultVal = true
      for(vc <- wvc(function.body, and(listOfpostCond)))
      {
        val checkResult = Validity.check(vc, listOfAssumption)
        logger.debug(s"validity check of $vc is $checkResult")
        resultExp = and(resultExp, checkResult)
        resultVal = resultVal && (if(checkResult.isInstanceOf[True]) true else false)
      }
      (resultExp, resultVal)
    }
    else
    {
      logger.debug(s"at verify, function ${function.name} has no post conditions")
      (False(), false)
    }
  }

  /**
    *
    * @param aSeq
    * @return
    */
  def flatten(aSeq:Sequence):Sequence=
  {
    def doFlatten(b: Sequence): List[Statement] = b match
    {
      case Sequence()                     => Nil
      case Sequence(a: Sequence, rest@_*) => doFlatten(a) ::: doFlatten(Sequence(rest: _*))
      case Sequence(a, rest@_*)           => a :: doFlatten(Sequence(rest: _*))
    }

    Sequence(doFlatten(aSeq):_*)
  }
}
