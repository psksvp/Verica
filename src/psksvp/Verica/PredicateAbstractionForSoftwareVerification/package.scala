package psksvp.Verica

/**
  * Created by psksvp on 13/05/2016.
  */
package object PredicateAbstractionForSoftwareVerification
{
  import psksvp.Verica.Lang._

  type AbstractDomain = List[Vector[Boolean]]

  /**
    *
    * @param a
    * @return
    */
  implicit def booleanVector2String(a:Vector[Boolean]):String=
  {
    var s = "("
    for(b <- a)
      s = s + (b + " ")
    s = s.trim + ")"
    s
  }

  /**
    *
    * @param a
    * @return
    */
  implicit def AbstractDomain2String(a:AbstractDomain):String = a match
  {
    case s :: rest => "[" + booleanVector2String(s) + " " + AbstractDomain2String(rest) + "]"
    case Nil       => ""
  }

  /**
    *
    * @param stmt
    * @return
    */
  def targets(stmt:Statement):List[Variable] = stmt match
  {
    case Assignment(variable, _)  => List(variable)
    //http://www.scala-lang.org/old/node/2758
    //http://stackoverflow.com/questions/31064753/how-pass-scala-array-into-scala-vararg-method
    case Sequence(stm, rest @ _*) => targets(stm) ::: targets(Sequence(rest: _*))
    case _                        => Nil

  }

  /**
    *
    * @param variables
    * @return
    */
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

  /**
    *
    * @param w
    * @return
    */
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
    *
    * @param q
    * @param s
    * @return
    */
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
    case Empty()                 => and(q, True())
    case Ensure(_)               => and(q, True())
    case VariableDeclaration(_,_)=> and(q, True())
  }

  /**
    *
    * @param s
    * @return
    */
  def traverse(s:Statement):Statement = s match
  {
    case Sequence(a, rest@_*) => traverse(a, Sequence(rest: _*))
    case _                    => traverse(Empty(), s)
  }

  /**
    *
    * @param c
    * @param s
    * @return
    */
  def traverse(c:Statement, s:Statement):Statement = s match
  {
    case Assignment(_, _)          => s
    case Assert(_)                 => s
    case Assume(_)                 => s
    case Ensure(_)                 => s
    case Return(_)                 => s
    case VariableDeclaration(_,_)  => s
    case Choice(a, b)              => Choice(traverse(c, a), traverse(c, b))
    case Sequence(a)               => traverse(c, a)
    case Sequence(a, rest@_*)      => val aP = traverse(c, a)
                                      val bP = traverse(Sequence(c, aP), Sequence(rest: _*))
                                      Sequence(aP, bP)
    case While(p, i, e, _)         => val (j, b) = infer(c, s)
                                      While(p, and(i, j), e, b)
  }

  /*
  def makePredicates(w:While, c:Statement):List[Predicate]=
  {
    val t = targets(w)
    val n = norm(True(), c)

  }*/

  /**
    *
    * @param c
    * @param s
    * @return
    */
  def infer(c:Statement, s:Statement):(Expression, Statement)= s match
  {
    case While(p, i, e, b) =>
      val h = havoc(targets(b))
      val n = norm(True(), c)
      var r = alpha(n, p)
      var j:Expression = True()
      var bp:Statement = Empty()
      var next:AbstractDomain = Nil
      var keepLooping = true
      var iteration = 0
      do
      {
        j  = gamma(r, p)
        val a  = Assume(and(e, i,  j))
        bp = traverse(Sequence(c, h, a), b)
        val q  = norm(True(), Sequence(c, h, a, bp))
        next = union(r, q, p)
        if(r != next)
          r = next
        else
          keepLooping = false
        iteration = iteration + 1
        //if(0 == iteration % 5)
        println(s"infer at iteration $iteration current is $j")
      }while(true == keepLooping)

      (j, bp)

    case _ => sys.error("expect parm s to be a While")
  }

  /**
    *
    * @param expr
    * @param pred
    * @return
    */
  def alpha(expr:Expression, pred:Predicates):AbstractDomain=
  {
    var ls:AbstractDomain = Nil
    val combinationSize = scala.math.pow(2, pred.count).toInt  // TODO: can have overflow problem
    for(i <- 0 until combinationSize)
    {
      val combination =  psksvp.booleanVector(i, pred.count)
      val expr2Chk = implies(expr, gamma(combination, pred))
      if(True() == Z3.Validity.check(expr2Chk))
      {
        ls = ls :+ combination
      }
    }

    ls
  }

  /**
    *
    * @param combination
    * @param pred
    * @return
    */
  def gamma(combination:Vector[Boolean], pred:Predicates):Expression=
  {
    require(combination.length == pred.count, "gamma error: absDomain.size != predicates.count")

    def makeExpression(b:Boolean, p:Predicate):Expression = if(b) p else not(p)

    if(1 == combination.length)
    {
      makeExpression(combination(0), pred(0))
    }
    else
    {
      var expr = makeExpression(combination(0), pred(0))
      for(i <- 1 until combination.length)
      {
        expr = and(expr, makeExpression(combination(i), pred(i)))
      }
      expr
    }
  }

  /**
    *
    * @param absDomain
    * @param pred
    * @return
    */
  def gamma(absDomain:AbstractDomain, pred:Predicates):Expression = absDomain match
  {
    case Nil       => sys.error("gamma(a:AbstractDomain, ..) a is Nil")
    case a :: Nil  => gamma(a, pred)
    case a :: rest => and(gamma(a, pred), gamma(rest, pred))
  }

  /**
    *
    * @param r
    * @param q
    * @param predicates
    * @return
    */
  def union(r:AbstractDomain, q:Expression, predicates: Predicates):AbstractDomain=
  {
    var result:AbstractDomain = Nil
    for(m <- r)
    {
      val expr = and(implies(r, m), implies(q, gamma(m, predicates)))
      if(True() == Z3.Validity.check(expr))
        result = result :+ m
    }
    r
  }

  /**
    *
    * @param b
    * @return
    */
  implicit def boolean2Expression(b:Boolean):Expression = if(b) True() else False()

  /**
    *
    * @param a
    * @return
    */
  implicit def booleanVectpr2Expression(a:Vector[Boolean]):Expression=
  {
    if(1 == a.length)
      a(0)
    else
    {
      var expr:Expression = a(0)
      for(i <- 1 until a.length)
      {
        expr = and(expr, a(i))
      }
      expr
    }
  }

  /**
    *
    * @param absDomain
    * @return
    */
  implicit def abstractDomain2Expression(absDomain:AbstractDomain):Expression = absDomain match
  {
    case Nil            => sys.error("abstractDomain2Expression(a:AbstractDomain, ..) a is Nil")
    case a :: Nil       => a
    case a :: b :: Nil  => or(a, b)
    case a :: b :: rest => or(or(a, b), abstractDomain2Expression(rest))
  }
}
