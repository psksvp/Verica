package psksvp.Verica

import psksvp.Verica.Z3.Validity

/**
  * Created by psksvp on 13/05/2016.
  */
package object PredicateAbstractionForSoftwareVerification extends com.typesafe.scalalogging.LazyLogging
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
    def makeAssignmentList(vars:List[Variable]):List[Assignment]=vars match
    {
      case v :: rest => List(Assignment(v, Variable("y"+vars.size))) ::: makeAssignmentList(rest)
      case _         => Nil
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
    case While(_, i, e, b) =>
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
    * @param f
    * @return
    */
  def traverse(f:Function):Function=
  {
    def filterForAll(ls:List[VerificationStatment]):List[VerificationStatment] = ls match
    {
      case Nil                                       => Nil
      case Assume(UniversalQuantifier(_, _)) :: rest => filterForAll(rest)
      case Ensure(_) :: rest                         => filterForAll(rest)
      case h :: rest                                 => h :: filterForAll(rest)
    }

    def margeVerificationStatements(ls:List[VerificationStatment], s:Statement):Sequence =
    {
      val nls = filterForAll(ls)//psksvp.removeElement[VerificationStatment, Ensure](ls)
      val m = Sequence(Sequence(nls:_*), s)
      flatten(m)
    }



    val nb = margeVerificationStatements(f.verificationStatments, f.body)
    //val nb = f.body
    val s = traverse(Empty(), nb)
    Function(f.name, f.parameters, f.typeClass, s, f.verificationStatments)
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
    case Assignment(v, e)          => s
    case Assert(_)                 => s
    case Assume(_)                 => s
    case Ensure(_)                 => s
    case Return(_)                 => s
    case VariableDeclaration(_,_)  => s
    case i:If                      => i.toChoice
    case Choice(a, b)              => Choice(traverse(c, a), traverse(c, b))
    case Sequence(a)               => traverse(c, a)
    case Sequence(a, rest@_*)      => val aP = traverse(c, a)
                                      val bP = traverse(Sequence(c, aP), Sequence(rest: _*))
                                      Sequence(aP, bP).flatten
    case w@While(p, i, e, body)    => if(p.count > 0)
                                      {
                                        val (j, b) = infer(c, w)
                                        While(p, and(i, j), e, b)
                                      }
                                      else
                                      {
                                        val prd = generatePredicates(w, c)
                                        val (j, b) = infer(c, While(prd, i, e, body))
                                        While(prd, and(i, j), e, b)
                                      }
  }


  /**
    *
    * @param w
    * @param c
    * @return
    */
  def generatePredicates(w:While, c:Statement):Predicates=
  {
    def traverseLocalContext(s:Statement):Map[Variable, Expression] = s match
    {
      case Sequence(Assignment(v, e), rest@_*) => Map(v -> e) ++ traverseLocalContext(Sequence(rest: _*))
      case Sequence(_, rest@_*)                => traverseLocalContext(Sequence(rest: _*))
      case Assignment(v, e)                    => Map(v -> e)
      case _                                   => Map.empty[Variable, Expression]
    }

    val cs = c match
    {
      case a:Sequence => flatten(a)
      case _          => c
    }

    val old = traverseLocalContext(cs)
    var result:List[Predicate] = Nil //List(w.expr)
    for(t <- targets(w.body))
    {
      val e:List[Expression] = t match
      {
        case Variable(_, _, ValueVariable()) if old.isDefinedAt(t) => val tn = s"${Prettified(t)}"
                                                                      List(s"$tn <= ${old(t)}",
                                                                           s"$tn >= ${old(t)}")

        case Variable(_, _, ArrayVariable())                       => List()
        case _                                                     => List()
      }
      result = result ::: e
    }
    Predicates(result:_*)
  }


  /*
  def infer2(c:Statement, w:While):(Expression, Statement)=
  {
    def traverseContext(s:Statement):Map[Variable, Expression] = s match
    {
      case Sequence(Assignment(v, e), rest@_*) => Map(v -> e) ++ traverseContext(Sequence(rest: _*))
      case Sequence(_, rest@_*)                => traverseContext(Sequence(rest: _*))
      case Assignment(v, e)                    => Map(v -> e)
      case _                                   => Map.empty[Variable, Expression]
    }

    val cs = c match
    {
      case a:Sequence => flatten(a)
      case _          => c
    }

    val old = traverseContext(cs)
    val lsPred = predicateCombinations(targets(w.body), old)
    for(predComb <- lsPred)
    {

    }
  } */

  /**
    *
    * @param c context: statements before statement s (while statement)
    * @param s a while statement
    * @return turple of inferred invariant and statement s
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
        println("----------")
        println(r)
        println(j)
        println("==========")
        val a  = Assume(and(e, i,  j))
        bp = traverse(Sequence(c, h, a), b)
        val q  = norm(True(), Sequence(c, h, a, bp))
        next = union(r, q, p)
        if(!next.isEmpty && r != next)
          r = next
        else
          keepLooping = false
        iteration = iteration + 1
        logger.debug(s"infer at iteration $iteration current is $j")
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
    case a :: rest => or(gamma(a, pred), gamma(rest, pred))
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

      //val rim = implies(r, m)
      //val qim = implies(q, gamma(m, predicates))
      //if(True() == Z3.Validity.check(rim) && True() == Z3.Validity.check(qim))
      //  result = result :+ m
    }
    result
  }



  def booleanVector(size:Int, fillWith:Boolean = true):Vector[Boolean] = Vector.fill[Boolean](size)(fillWith)

  /*
   * TODO: does not work.
   */
  /*
  def union2(r:AbstractDomain, q:Expression, predicates: Predicates):AbstractDomain=
  {
    var result:AbstractDomain = List(booleanVector(r(0).size))
    for(m <- r)
    {
      val expr = and(not(implies(result, m)),
                     implies(r, m),
                     implies(q, gamma(m, predicates)))
      if(True() == Z3.Validity.check(expr))
      {
        var c = m.toSet // c is Set[Boolean], m is Vector[Boolean]
        for (l <- m) // l is a Boolean of a predicate
        {
          val d = c diff Set(l)
          if(True() == Validity.check(and(implies(r, d),
                                          implies(q, gamma(d, predicates)))))
          {
            c = d
          }
        }
        result = result :+ m
      }
    }
    result
  } */

  /**
    *
    * @param s
    * @tparam T
    * @return
    */
  implicit def set2Vector[T](s:Set[T]):Vector[T]=s.toVector
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
  implicit def booleanVector2Expression(a:Vector[Boolean]):Expression=
  {
    if(1 == a.length)
      a(0)
    else if(a.length > 1)
    {
      var expr:Expression = a(0)
      for(i <- 1 until a.length)
      {
        expr = and(expr, a(i))
      }
      expr
    }
    else
    {
      sys.error("psksvp.booleanVector2Expression(a) a is empty")
    }
  }


  /**
    *
    * @param absDomain
    * @return
    */
  implicit def abstractDomain2Expression(absDomain:AbstractDomain):Expression = absDomain match
  {
    case Nil       => sys.error("abstractDomain2Expression(a:AbstractDomain, ..) a is Nil")
    case a :: Nil  => a
    case a :: rest => or(a, abstractDomain2Expression(rest))
  }

  /**
    *
    * @param targets
    * @param oldValue
    * @return
    */
  def predicateCombinations(targets:List[Variable], oldValue:Map[Variable, Expression]):List[List[Expression]] =
  {
    /**
      *
      * @param v
      * @param lsOP
      * @return
      */
    def makePredicates(v:Variable, lsOP:List[String] = List("<","<=",">",">=","==","!=")):List[Expression]=lsOP match
    {
      case Nil         => Nil
      case op :: rest  => List[Expression](s"$v $op ${oldValue(v)}") ::: makePredicates(v, rest)
    }

    /**
      *
      * @param tls
      * @return
      */
    def targetPredicates(tls:List[Variable]):List[List[Expression]]=tls match
    {
      case Nil       => Nil
      case v :: rest => List(makePredicates(v)) ::: targetPredicates(rest)
    }


    ////////////////////
    val tp = targetPredicates(targets)
    val mk = psksvp.crossProduct(tp)

    mk
  }
}
