package psksvp.Verica

import psksvp.Verica.Z3.Validity

/**
  * Created by psksvp on 13/05/2016.
  */
package object PredicateAbstraction extends com.typesafe.scalalogging.LazyLogging
{
  import psksvp.Verica.Lang._

  type AbstractDomain = Expression

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
    case Return(_)               => and(q, True())
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
                                      Sequence(aP, bP)//.flatten
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
      println("context :" + n)
      println("predicate :" + p)
      var r = alpha(n, p)
      var j:Expression = True()
      var bp:Statement = Empty()
      var keepLooping = true
      var iteration = 0
      do
      {
        j  = gamma(r)
        val a  = Assume(and(e, i,  j))
        bp = traverse(Sequence(c, h, a), b)
        val q  = norm(True(), Sequence(c, h, a, bp))
//        //no havac uncomment below
//        bp = traverse(Sequence(c, a), b)
//        val q  = norm(True(), Sequence(c, a, bp))
        val next = union(r, q, p)
        println("next is"  + next)
        println("   r is"  + r)
        if(r != next)
          r = next
        else
          keepLooping = false
        iteration = iteration + 1
        logger.debug(s"infer at iteration $iteration current is $j")
      }while(keepLooping)

      (j, bp)

    case _ => sys.error("expect parm s to be a While")
  }

  /**
    *
    * @param expr
    * @param pred
    * @return
    */
  def alpha(expr:Expression, pred:Predicates):AbstractDomain = union(False(), expr, pred)

  /**
    *
    * @param absDomain
    * @return
    */
  def gamma(absDomain:AbstractDomain):Expression = absDomain

  /**
    *
    * @param r
    * @param q
    * @param predicates
    * @return
    */
  def union(r:AbstractDomain, q:Expression, predicates: Predicates):AbstractDomain =
  {
    def makeExpression(b:Vector[Boolean], p:Predicates):Expression =
    {
      require(b.size == p.count, "unionX makeExpression, size of b != p.count")
      var expr:Expression = if(b(0)) p(0) else not(p(0))
      for(i <- 1 until p.count)
      {
        expr = or(expr, if(b(i)) p(i) else not(p(i)))
      }
      expr
    }

    var result:Expression = True()
    val combinationSize = scala.math.pow(2, predicates.count).toInt  // TODO: can have overflow problem
    for(i <- 0 until combinationSize)
    {
      val combination = psksvp.booleanVector(i, predicates.count)
      val m = makeExpression(combination, predicates)
      val qExpr = implies(q, m)
      val rExpr = implies(r, m)
      val expr = and(rExpr, qExpr)

      if (True() == Z3.Validity.check(expr))
      {
        result = and(result, m)
        println("union check rtn  True(): " + expr)
      }
      else
        println("union check rtn False(): " + expr)
    }
    result
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
