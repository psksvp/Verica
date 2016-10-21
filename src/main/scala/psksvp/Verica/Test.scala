package psksvp.Verica

import psksvp.Verica.Z3.{Exists, QE, SuchThat, Validity}
import psksvp.Verica.PredicateAbstraction._



/**
  * Created by psksvp on 11/04/2016.
  */
object Test
{

  import psksvp.Verica.Lang._

  val helloworld =
    """
      |module(HelloWorld)
      |{
      |  function sum(n:Integer):Integer
      |  [assume(n > 0)]
      |  {
      |    var i:Integer
      |    var s:Integer
      |    i := 1
      |    s := 0
      |    while(i <= n, [(i >= 1)(i = s + 1)(i<1)(s>=0), true])
      |    {
      |     s := s + i
      |     i := i + 1
      |    }
      |    return(s)
      |  }
      |
      |  function sumArray(a:Array<Integer>):Integer
      |  [assume(a.length >= 0),
      |   assume(forAll(j, a[j] >= 0)),
      |   ensure(r >= 0)]
      |  {
      |    var i:Integer
      |    var s:Integer
      |    i := 0
      |    s := 0
      |    while(i < a.length, [(i >= 0)(i < a.length)(s >= 0), true])
      |    {
      |      s := s + a[i]
      |      i := i + 1
      |    }
      |    return(s)
      |  }
      |
      |  function testing(a:Array<Integer>):Integer
      |  {
      |    var i:Integer
      |    var s:Integer
      |    i := 0
      |    s := 0
      |    while(i < a.length, [(i >= 0)(i < a.length)(s >= 0), true])
      |    {
      |      s := s + a[i]
      |      i := i + 1
      |      k := HelloWorld.sum(s)
      |    }
      |    io.println(1+2, math.cos(23), i /\ j)
      |    return(s)
      |  }
      |}
    """.stripMargin

  def testParsing: Unit =
  {
    val wh = Parser.parse(helloworld)
    println(wh)
  }

  def testInfer: Unit =
  {
    val wh = Parser.parse(helloworld)
    println(wh)
    val afterInfer = wh.function("sum") match
    {
      case Some(f) => traverse(f.body)
      case None => sys.error("function sum does not exist")
    }

    println(afterInfer)
  }

  def testVC: Unit =
  {
    awp("{R := R + Y  Q := Q + 1}", "x == R + Y * Q")

    val prg: Statement =
      """
        |{
        |  r := x
        |  q := 0
        |  while(y <= r, [(true), x = r + y * q])
        |  {
        |    r := r - y
        |    q := q + 1
        |  }
        |}
      """.stripMargin

    val gg = awp(prg, """x == r + y * q /\ r < y""")
    println(gg)

    val kk = wvc(prg, """x == r + y * q /\ r < y""")
    for (f <- kk)
      println(f + " is " + Validity.check(f))


    println(strongestPostCondition("{x:=0 y:=0}", True()))
  }

  def testInferWithArray: Unit =
  {
    val f1: Function =
      """
        |  function sumArray(a:Array<Integer>):Integer
        |  [assume(a.length > 0),
        |   assume(forAll(j, a[j] > 0)),
        |   ensure(r > 0)]
        |  {
        |    var i:Integer
        |    var r:Integer
        |    var j:Integer
        |    i := 0
        |    r := 0
        |    while(i < a.length, [(i >= 0)(i <= 0)(r >= 0)(r <= 0),true])
        |    {
        |      r := r + a[i]
        |      i := i + 1
        |    }
        |  }
      """.stripMargin


    val fl: Function =
      """
        |  function sumArray(n:Integer):Integer
        |  [assume(n > 0), ensure(r <= 0 /\ i >= 0)]
        |  {
        |    i := 0
        |    r := 0
        |    while(i < n, [,true])
        |    {
        |      r := r - i
        |      i := i + 1
        |    }
        |
        |  }
      """.stripMargin

    println(fl)
    //println(strongestPostCondition(f1.body, True()))
    val f2 = traverse(fl)
    println(f2)
    println(verify(f2))

    //val a = assumptionOf(f1)
    //println(Z3.makeAssumptions("sOlver", a))
  }

  def testVerifyFindMax:Unit=
  {
    val maxFunc:Function =
      """
        |function max(a:Array<Integer>):Integer
        |[assume(a.length >= 1),
        | ensure(forAll(j, (j >= 0 /\ j <= a.length) -> (a[j] <= result) ))]
        |{
        |   var i:Integer
        |   var result:Integer
        |   i := 1
        |   r := a[0]
        |   while(i < a.length, [(true), forAll(j, (j >= 0 /\ j <= i) -> (a[j] <= result))])
        |   {
        |     if(a[i] > result)
        |     {
        |       result := a[i]
        |     }
        |   }
        |}
      """.stripMargin

    println(maxFunc)
    println(verify(maxFunc))
  }

  def testPythonize: Unit=
  {
    val mm:Expression = """(a < b.length) /\ (i > 0)"""
    println(mm)
    println(Z3.pythonize(mm))
  }

  def testSP:Unit=
  {
    val m = strongestPostCondition("x := x + 1", """x > 0""")
    println(m)
    val toCheck = implies(and("""x > 0 """, m), "x >= 1")
    println(toCheck)
    println(Z3.Satisfiable.check(toCheck))
    println(Z3.Validity.check(toCheck))
  }

  def testCrazyLoop:Unit=
  {
    val f1: Function =
      """
        |  function sumArray(a:Array<Integer>):Integer
        |  [assume(a.length > 0),
        |   assume(forAll(j, a[j] >= 0)),
        |   ensure(r >= 0)]
        |  {
        |    var i:Integer
        |    var r:Integer
        |    i := 0
        |    r := 0
        |    while(i < a.length, [,true])
        |    {
        |      r := r + a[i]
        |    }
        |    return(r)
        |  }
      """.stripMargin
  }

  def testInferOnTrace: Unit =
  {
    val f1: Function =
      """
        |  function aTrace(n:Integer):Integer
        |  [assume(n > 0), ensure(k <= 0 /\ i >= 0 /\ j <= 20)]
        |  {
        |    i := 0
        |    j := 20
        |    k := 0
        |    while(i < n, [,true])
        |    {
        |      k := k - i
        |      i := i + 1
        |      j := j - 1
        |    }
        |  }
      """.stripMargin

    val fx:Function =
    """
      |function aFunc(n:Integer):Integer
      |[assume(n > 0), ensure(i >= 0 /\ j <= 0)]
      |{
      |  i := 0
      |  j := 0
      |  while(i <= n, [,true])
      |  {
      |    i := i + 1
      |    j := j - 1
      |  }
      |}
    """.stripMargin

    val fl:Function =
      """
        |function aFunc(n:Integer):Integer
        |[assume(n > 0), ensure(i >= 0)]
        |{
        |  var i:Integer
        |  var two:Integer
        |  var add:Integer
        |  i := 0
        |  while(i <= n, [(i >= 0)(i <= 0),true])
        |  {
        |    two := i
        |    add := two + 1
        |    i := add
        |  }
        |}
      """.stripMargin


    println(fx)
    val f2 = traverse(fx)
    println(f2)
    println(verify(f2))
  }

  def testAnotherTrace:Unit=
  {
    val fl:Function =
      """
        |function aFunc(n:Integer):Integer
        |[assume(n > 0), ensure(i >= 0 /\ j <= 0)]
        |{
        |  var i:Integer
        |  var j:Integer
        |  i := 0
        |  j := 0
        |  while(i <= n, [,true])
        |  {
        |    i := i + 1
        |    j := j - 1
        |  }
        |}
      """.stripMargin

    val fx:Function =
      """
        |function aFunc(n:Integer):Integer
        |[assume(j > 0), assume(i > 0)]
        |{
        |  t := i
        |  div := 0
        |  while(t >= j, [,true])
        |  {
        |    div := div + 1
        |    t := t - j
        |  }
        |}
      """.stripMargin

    val fy:Function =
      """
        |function aFunc(n:Integer):Integer
        |[assume(N > 0)]
        |{
        |  j := N
        |  while(j < N, [,true])
        |  {
        |    j := j + 1
        |  }
        |}
      """.stripMargin

    val fz:Function =
      """
        |function aFunc(n:Integer):Integer
        |[assume(N > 0)]
        |{
        |  i := 0
        |  x := i
        |  while(i < 50, [,true])
        |  {
        |    i := i + 1
        |    x := x + i
        |  }
        |}
      """.stripMargin


    println(fz)
    val f2 = traverse(fz)
    println(f2)
    //println(verify(f2))
  }




  def main(args:Array[String]):Unit=
  {
    val pyListerner = new psksvp.Python.Listener
    pyListerner.start
    val start = System.nanoTime()

    val exprs:List[Expression] = List("x == 0", "j == 0", " ~(x < 100)", "j != 0")
    //al exprs:List[Expression] = List("x1==0", "y1==0", "x2=x1+1", "y1 != 0")
    //val exprs:List[Expression] = List("i==0", "i>=1000", "i > 1000")
    println(Z3.Interpolant.compute(exprs))
    println(Z3.Interpolant.checkTrace(exprs))
//    val e1 = booleanMinimize(List(0,1,2,3,4,7,6,8,11,13,15), List("p0", "p1", "p2", "p3"))
//    println(e1)
//    println(toCNF(e1))
//    println(toDNF(e1))
//    val e2 = booleanMinimize(List(1,2,3), List("p0", "p1"))
//    println(e2)
//    println(toCNF(e2))
//    println(toDNF(e2))
//
//    println(Validity.check("""(x > 0 /\ x >= 2) -> (x >= 2)"""))
//
//    println(alpha("""j == 0 /\ n > 0""", "(j >= 0)(j <= 0)"))
    //println(QE.solve(Exists(Variable("x") :: Nil), SuchThat("x * 2 + y == 4")))
    //testSP
    //testInferWithArray
    //testVerifyFindMax

    //testInferOnTrace
    testAnotherTrace
    //hardCode

    //hardCode2

    //val t:Expression = "And(p1, Or(p2, ~p3))"
    //println(t)

    //val t:Expression = """true /\ (p1 \/ ~p2) /\ (p1 \/ p2)"""
    //println(SymPy.symplify(t))
    println(System.nanoTime() - start)
    pyListerner.stop
  }

  def hardCode2:Unit=
  {
    val context:Expression = """retval2 == 0 /\ n2 == 10 /\ a2 == 0 /\ i2 == 0  /\ j2 == 0"""
    val r = alpha(context, "(i2 >= 0)(i2 <= 0)(j2 >= 0)(j2 <= 0)")
    println("-------------------------------------------")
    println(r)
    var simpR = gamma(r, "(i2 >= 0)(i2 <= 0)(j2 >= 0)(j2 <= 0)")
//    println(simpR)
////    println(abstractDomain2BooleanExpression(r))
//    val simp = SymPy.symplify(r)
//    println(simp)
//    simpR = booleanExpression2PredicateExpression(simp, "(i2 >= 0)(i2 <= 0)(j2 >= 0)(j2 <= 0)")
//    println(simpR)
//    println("-------------------------------------------")

    val decl:Statement = "var cmp1:Boolean"
    val guardAndBody:Expression =
      """
        |zero1 == i2 /\
        |one1 == n2 /\
        |cmp1 == (zero1 <= one1) /\
        |two1 == i2 /\
        |add1 == (two1 + 1) /\
        |i3 == add1 /\
        |three1 == j2 /\
        |sub1 == (three1 - 1) /\
        |j3 == sub1
      """.stripMargin

    var q = QE.solve(Exists(List(Variable("i2"), Variable("j2"))), SuchThat(and(guardAndBody, simpR, "n2 == 10")))
    println("-------------------------------------------")
    println(q)
    println("-------------------------------------------")


    //q = """((one1 = n2) /\ ((cmp1 = (zero1 <= one1)) /\ (cmp1 = true /\ ((add1 = (1 + two1)) /\ ((i3 = add1) /\ ((sub1 = (-1 + three1)) /\ ((j3 = sub1) /\ (((two1 + (-1 * zero1)) <= 0) /\ (((zero1 + (-1 * two1)) <= 0) /\ (((zero1 <= -1) \/ ((zero1 >= 1) \/ ((three1 <= -1) \/ (three1 <= 0)))) /\ (((zero1 <= -1) \/ ((zero1 >= 1) \/ ((three1 >= 0) \/ (three1 >= 1)))) /\ (((zero1 <= -1) \/ ((zero1 >= 1) \/ ((three1 >= 0) \/ (three1 <= 0)))) /\ (((zero1 <= -1) \/ ((zero1 <= 0) \/ ((three1 <= -1) \/ (three1 >= 1)))) /\ (((zero1 <= -1) \/ ((zero1 <= 0) \/ ((three1 <= -1) \/ (three1 <= 0)))) /\ (((zero1 <= -1) \/ ((zero1 <= 0) \/ ((three1 >= 0) \/ (three1 >= 1)))) /\ (((zero1 <= -1) \/ ((zero1 <= 0) \/ ((three1 >= 0) \/ (three1 <= 0)))) /\ (((zero1 >= 0) \/ ((zero1 >= 1) \/ ((three1 <= -1) \/ (three1 >= 1)))) /\ (((zero1 >= 0) \/ ((zero1 >= 1) \/ ((three1 <= -1) \/ (three1 <= 0)))) /\ (((zero1 >= 0) \/ ((zero1 >= 1) \/ ((three1 >= 0) \/ (three1 >= 1)))) /\ (((zero1 >= 0) \/ ((zero1 >= 1) \/ ((three1 >= 0) \/ (three1 <= 0)))) /\ (((zero1 >= 0) \/ ((zero1 <= 0) \/ ((three1 <= -1) \/ (three1 >= 1)))) /\ (((zero1 >= 0) \/ ((zero1 <= 0) \/ ((three1 <= -1) \/ (three1 <= 0)))) /\ (((zero1 >= 0) \/ ((zero1 <= 0) \/ ((three1 >= 0) \/ (three1 >= 1)))) /\ ((zero1 >= 0) \/ ((zero1 <= 0) \/ ((three1 >= 0) \/ (three1 <= 0))))))))))))))))))))))))))) """

    //var rPrime:Expression ="""((i3 >= 0) /\ ((i3 <= 0) /\ ((j3 >= 0) /\ (j3 <= 0))))"""
    var rPrime:Expression = """(((((((((((((((true /\ (((~(i3 >= 0) \/ ~(i3 <= 0)) \/ ~(j3 >= 0)) \/ (j3 <= 0))) /\ (((~(i3 >= 0) \/ ~(i3 <= 0)) \/ (j3 >= 0)) \/ ~(j3 <= 0))) /\ (((~(i3 >= 0) \/ ~(i3 <= 0)) \/ (j3 >= 0)) \/ (j3 <= 0))) /\ (((~(i3 >= 0) \/ (i3 <= 0)) \/ ~(j3 >= 0)) \/ ~(j3 <= 0))) /\ (((~(i3 >= 0) \/ (i3 <= 0)) \/ ~(j3 >= 0)) \/ (j3 <= 0))) /\ (((~(i3 >= 0) \/ (i3 <= 0)) \/ (j3 >= 0)) \/ ~(j3 <= 0))) /\ (((~(i3 >= 0) \/ (i3 <= 0)) \/ (j3 >= 0)) \/ (j3 <= 0))) /\ ((((i3 >= 0) \/ ~(i3 <= 0)) \/ ~(j3 >= 0)) \/ ~(j3 <= 0))) /\ ((((i3 >= 0) \/ ~(i3 <= 0)) \/ ~(j3 >= 0)) \/ (j3 <= 0))) /\ ((((i3 >= 0) \/ ~(i3 <= 0)) \/ (j3 >= 0)) \/ ~(j3 <= 0))) /\ ((((i3 >= 0) \/ ~(i3 <= 0)) \/ (j3 >= 0)) \/ (j3 <= 0))) /\ ((((i3 >= 0) \/ (i3 <= 0)) \/ ~(j3 >= 0)) \/ ~(j3 <= 0))) /\ ((((i3 >= 0) \/ (i3 <= 0)) \/ ~(j3 >= 0)) \/ (j3 <= 0))) /\ ((((i3 >= 0) \/ (i3 <= 0)) \/ (j3 >= 0)) \/ ~(j3 <= 0))) /\ ((((i3 >= 0) \/ (i3 <= 0)) \/ (j3 >= 0)) \/ (j3 <= 0)))"""
    //var rPrime:Expression = """(i2 >= 0) /\ (i2 <= 0) /\ (j2 >= 0) /\ (j2 <= 0)"""

    var theDomain:AbstractDomain = union(rPrime, q, "(i3 >= 0)(i3 <= 0)(j3 >= 0)(j3 <= 0)")
    var next = abstractDomain2PredicateExpression(theDomain, "(i3 >= 0)(i3 <= 0)(j3 >= 0)(j3 <= 0)")

    while(rPrime != next)
    {
      rPrime = next
      theDomain = union(rPrime, q, "(i3 >= 0)(i3 <= 0)(j3 >= 0)(j3 <= 0)")
      next = abstractDomain2PredicateExpression(theDomain,"(i3 >= 0)(i3 <= 0)(j3 >= 0)(j3 <= 0)")
      println("-----iteration----- ")
    }

    println("-------------------------------------------")
    println(next)
    println(theDomain)
    println(gamma(theDomain, "(i3 >= 0)(i3 <= 0)(j3 >= 0)(j3 <= 0)"))
    //println(SymPy.symplify(theDomain))
    //println(booleanExpression2PredicateExpression(SymPy.symplify(theDomain), "(i3 >= 0)(i3 <= 0)(j3 >= 0)(j3 <= 0)"))
  }
//
  ////////////////////////////////////////////////////////////////////
  def hardCode:Unit=
  {
    println("--------------------------------------------")

    val context:Expression =
          """
            |retval2 == 0 /\ n2 == 10 /\ a2 == 0 /\ i2 == 0
          """.stripMargin

    var r = alpha(context, "(i2 >= 0)(i2 <= 0)")
    //val simpR = abstractDomain2PredicateExpression(r, "(i2 >= 0)(i2 <= 0)")
    val simpR = and("i2 >= 0", "i2 <= 0")
    //val r:Expression = "i2 == 0"
    println(r)

    val decl:Statement = "var cmp1:Boolean"

    val guardAndBody:Expression =
     """
        |zero1 == i2 /\
        |one1 == n2 /\
        |cmp1 == (zero1 <= one1) /\
        |true == cmp1 /\
        |two1 == i2 /\
        |add1 == (two1 + 1) /\
        |i3 == add1
      """.stripMargin

    val varsLastIndexInTerm = List(Variable("i3"),
                                   Variable("zero1"),
                                   Variable("one1"),
                                   Variable("n2"),
                                   Variable("cmp1"),
                                   Variable("two1"),
                                   Variable("add1"))

    val diff = vars(guardAndBody).toSet diff varsLastIndexInTerm.toSet

    println(diff)

    val m = QE.solve(Exists(diff.toSeq), SuchThat(and(guardAndBody, simpR)))  // use r because i2 is index 2 is entry point index of i
    println("m is :" + m)

    println("-------------------------------")

    val mHash:Expression = """one == n /\ cmp == (zero <= one) /\ true == cmp /\ add == 1 + two /\ i == add /\ zero <= 0 /\ zero >= 0 /\ two <= 0 /\ two >= 0"""

    //rename r to rHash (((true /\ (~(i2 >= 0) \/ (i2 <= 0))) /\ ((i2 >= 0) \/ ~(i2 <= 0))) /\ ((i2 >= 0) \/ (i2 <= 0)))
    //var rHash:Expression = """(((true /\ (~(i >= 0) \/ (i <= 0))) /\ ((i >= 0) \/ ~(i <= 0))) /\ ((i >= 0) \/ (i <= 0)))"""
    //var rHash:Expression = "i == 0"
    var rHash:Expression = """(i >= 0) /\ (i <= 0)"""
    //var rHash:Expression = """(((true /\ (~(i >= 0) \/ (i <= 0))) /\ ((i >= 0) \/ ~(i <= 0))) /\ ((i >= 0) \/ (i <= 0)))"""
    //rename context
    var contextHash:Expression =
      """
        |retval == 0 /\ n == 10 /\ a == 0 /\ i == 0
      """.stripMargin

    println("-----------------------------------------------------------")
    val q = mHash //and(contextHash, rHash, mHash)
    var theDomain = union(rHash, q, "(i >= 0)(i <= 0)")
    var next = abstractDomain2PredicateExpression(theDomain, "(i >= 0)(i <= 0)")

    println("next: " + next)
    println("rHash:" + rHash)
    println(rHash == next)

    rHash = next

    println("-----------------------------------------------------------")
    //q = and(contextHash, rHash, mHash)
    theDomain = union(rHash, q, "(i >= 0)(i <= 0)")
    next = abstractDomain2PredicateExpression(theDomain, "(i >= 0)(i <= 0)")

    println("next: " + next)
    println("rHash:" + rHash)
    println(rHash == next)

    //println(SymPy.symplify(theDomain))

  }
}






//    import psksvp.Verica.Z3._
//    val exp:Expression =
//      """
//        |(true /\ (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((true /\ (((((~(k <= 0) \/ ~(k >= 0)) \/ ~(m <= 1)) \/ ~(m >= 1)) \/ ~(i <= 0)) \/ (i >= 0))) /\ (((((~(k <= 0) \/ ~(k >= 0)) \/ ~(m <= 1)) \/ ~(m >= 1)) \/ (i <= 0)) \/ (i >= 0))) /\ (((((~(k <= 0) \/ ~(k >= 0)) \/ ~(m <= 1)) \/ (m >= 1)) \/ ~(i <= 0)) \/ ~(i >= 0))) /\ (((((~(k <= 0) \/ ~(k >= 0)) \/ ~(m <= 1)) \/ (m >= 1)) \/ ~(i <= 0)) \/ (i >= 0))) /\ (((((~(k <= 0) \/ ~(k >= 0)) \/ ~(m <= 1)) \/ (m >= 1)) \/ (i <= 0)) \/ ~(i >= 0))) /\ (((((~(k <= 0) \/ ~(k >= 0)) \/ ~(m <= 1)) \/ (m >= 1)) \/ (i <= 0)) \/ (i >= 0))) /\ (((((~(k <= 0) \/ ~(k >= 0)) \/ (m <= 1)) \/ ~(m >= 1)) \/ ~(i <= 0)) \/ ~(i >= 0))) /\ (((((~(k <= 0) \/ ~(k >= 0)) \/ (m <= 1)) \/ ~(m >= 1)) \/ ~(i <= 0)) \/ (i >= 0))) /\ (((((~(k <= 0) \/ ~(k >= 0)) \/ (m <= 1)) \/ ~(m >= 1)) \/ (i <= 0)) \/ ~(i >= 0))) /\ (((((~(k <= 0) \/ ~(k >= 0)) \/ (m <= 1)) \/ ~(m >= 1)) \/ (i <= 0)) \/ (i >= 0))) /\ (((((~(k <= 0) \/ ~(k >= 0)) \/ (m <= 1)) \/ (m >= 1)) \/ ~(i <= 0)) \/ ~(i >= 0))) /\ (((((~(k <= 0) \/ ~(k >= 0)) \/ (m <= 1)) \/ (m >= 1)) \/ ~(i <= 0)) \/ (i >= 0))) /\ (((((~(k <= 0) \/ ~(k >= 0)) \/ (m <= 1)) \/ (m >= 1)) \/ (i <= 0)) \/ ~(i >= 0))) /\ (((((~(k <= 0) \/ ~(k >= 0)) \/ (m <= 1)) \/ (m >= 1)) \/ (i <= 0)) \/ (i >= 0))) /\ (((((~(k <= 0) \/ (k >= 0)) \/ ~(m <= 1)) \/ ~(m >= 1)) \/ ~(i <= 0)) \/ ~(i >= 0))) /\ (((((~(k <= 0) \/ (k >= 0)) \/ ~(m <= 1)) \/ ~(m >= 1)) \/ ~(i <= 0)) \/ (i >= 0))) /\ (((((~(k <= 0) \/ (k >= 0)) \/ ~(m <= 1)) \/ ~(m >= 1)) \/ (i <= 0)) \/ ~(i >= 0))) /\ (((((~(k <= 0) \/ (k >= 0)) \/ ~(m <= 1)) \/ ~(m >= 1)) \/ (i <= 0)) \/ (i >= 0))) /\ (((((~(k <= 0) \/ (k >= 0)) \/ ~(m <= 1)) \/ (m >= 1)) \/ ~(i <= 0)) \/ ~(i >= 0))) /\ (((((~(k <= 0) \/ (k >= 0)) \/ ~(m <= 1)) \/ (m >= 1)) \/ ~(i <= 0)) \/ (i >= 0))) /\ (((((~(k <= 0) \/ (k >= 0)) \/ ~(m <= 1)) \/ (m >= 1)) \/ (i <= 0)) \/ ~(i >= 0))) /\ (((((~(k <= 0) \/ (k >= 0)) \/ ~(m <= 1)) \/ (m >= 1)) \/ (i <= 0)) \/ (i >= 0))) /\ (((((~(k <= 0) \/ (k >= 0)) \/ (m <= 1)) \/ ~(m >= 1)) \/ ~(i <= 0)) \/ ~(i >= 0))) /\ (((((~(k <= 0) \/ (k >= 0)) \/ (m <= 1)) \/ ~(m >= 1)) \/ ~(i <= 0)) \/ (i >= 0))) /\ (((((~(k <= 0) \/ (k >= 0)) \/ (m <= 1)) \/ ~(m >= 1)) \/ (i <= 0)) \/ (i >= 0))) /\ (((((~(k <= 0) \/ (k >= 0)) \/ (m <= 1)) \/ (m >= 1)) \/ ~(i <= 0)) \/ ~(i >= 0))) /\ (((((~(k <= 0) \/ (k >= 0)) \/ (m <= 1)) \/ (m >= 1)) \/ ~(i <= 0)) \/ (i >= 0))) /\ (((((~(k <= 0) \/ (k >= 0)) \/ (m <= 1)) \/ (m >= 1)) \/ (i <= 0)) \/ ~(i >= 0))) /\ (((((~(k <= 0) \/ (k >= 0)) \/ (m <= 1)) \/ (m >= 1)) \/ (i <= 0)) \/ (i >= 0))) /\ ((((((k <= 0) \/ ~(k >= 0)) \/ ~(m <= 1)) \/ ~(m >= 1)) \/ ~(i <= 0)) \/ ~(i >= 0))) /\ ((((((k <= 0) \/ ~(k >= 0)) \/ ~(m <= 1)) \/ ~(m >= 1)) \/ ~(i <= 0)) \/ (i >= 0))) /\ ((((((k <= 0) \/ ~(k >= 0)) \/ ~(m <= 1)) \/ ~(m >= 1)) \/ (i <= 0)) \/ ~(i >= 0))) /\ ((((((k <= 0) \/ ~(k >= 0)) \/ ~(m <= 1)) \/ ~(m >= 1)) \/ (i <= 0)) \/ (i >= 0))) /\ ((((((k <= 0) \/ ~(k >= 0)) \/ ~(m <= 1)) \/ (m >= 1)) \/ ~(i <= 0)) \/ ~(i >= 0))) /\ ((((((k <= 0) \/ ~(k >= 0)) \/ ~(m <= 1)) \/ (m >= 1)) \/ ~(i <= 0)) \/ (i >= 0))) /\ ((((((k <= 0) \/ ~(k >= 0)) \/ ~(m <= 1)) \/ (m >= 1)) \/ (i <= 0)) \/ ~(i >= 0))) /\ ((((((k <= 0) \/ ~(k >= 0)) \/ ~(m <= 1)) \/ (m >= 1)) \/ (i <= 0)) \/ (i >= 0))) /\ ((((((k <= 0) \/ ~(k >= 0)) \/ (m <= 1)) \/ ~(m >= 1)) \/ ~(i <= 0)) \/ ~(i >= 0))) /\ ((((((k <= 0) \/ ~(k >= 0)) \/ (m <= 1)) \/ ~(m >= 1)) \/ ~(i <= 0)) \/ (i >= 0))) /\ ((((((k <= 0) \/ ~(k >= 0)) \/ (m <= 1)) \/ ~(m >= 1)) \/ (i <= 0)) \/ ~(i >= 0))) /\ ((((((k <= 0) \/ ~(k >= 0)) \/ (m <= 1)) \/ ~(m >= 1)) \/ (i <= 0)) \/ (i >= 0))) /\ ((((((k <= 0) \/ ~(k >= 0)) \/ (m <= 1)) \/ (m >= 1)) \/ ~(i <= 0)) \/ ~(i >= 0))) /\ ((((((k <= 0) \/ ~(k >= 0)) \/ (m <= 1)) \/ (m >= 1)) \/ ~(i <= 0)) \/ (i >= 0))) /\ ((((((k <= 0) \/ ~(k >= 0)) \/ (m <= 1)) \/ (m >= 1)) \/ (i <= 0)) \/ ~(i >= 0))) /\ ((((((k <= 0) \/ ~(k >= 0)) \/ (m <= 1)) \/ (m >= 1)) \/ (i <= 0)) \/ (i >= 0))) /\ ((((((k <= 0) \/ (k >= 0)) \/ ~(m <= 1)) \/ ~(m >= 1)) \/ ~(i <= 0)) \/ ~(i >= 0))) /\ ((((((k <= 0) \/ (k >= 0)) \/ ~(m <= 1)) \/ ~(m >= 1)) \/ ~(i <= 0)) \/ (i >= 0))) /\ ((((((k <= 0) \/ (k >= 0)) \/ ~(m <= 1)) \/ ~(m >= 1)) \/ (i <= 0)) \/ ~(i >= 0))) /\ ((((((k <= 0) \/ (k >= 0)) \/ ~(m <= 1)) \/ ~(m >= 1)) \/ (i <= 0)) \/ (i >= 0))) /\ ((((((k <= 0) \/ (k >= 0)) \/ ~(m <= 1)) \/ (m >= 1)) \/ ~(i <= 0)) \/ ~(i >= 0))) /\ ((((((k <= 0) \/ (k >= 0)) \/ ~(m <= 1)) \/ (m >= 1)) \/ ~(i <= 0)) \/ (i >= 0))) /\ ((((((k <= 0) \/ (k >= 0)) \/ ~(m <= 1)) \/ (m >= 1)) \/ (i <= 0)) \/ ~(i >= 0))) /\ ((((((k <= 0) \/ (k >= 0)) \/ ~(m <= 1)) \/ (m >= 1)) \/ (i <= 0)) \/ (i >= 0))) /\ ((((((k <= 0) \/ (k >= 0)) \/ (m <= 1)) \/ ~(m >= 1)) \/ ~(i <= 0)) \/ ~(i >= 0))) /\ ((((((k <= 0) \/ (k >= 0)) \/ (m <= 1)) \/ ~(m >= 1)) \/ ~(i <= 0)) \/ (i >= 0))) /\ ((((((k <= 0) \/ (k >= 0)) \/ (m <= 1)) \/ ~(m >= 1)) \/ (i <= 0)) \/ ~(i >= 0))) /\ ((((((k <= 0) \/ (k >= 0)) \/ (m <= 1)) \/ ~(m >= 1)) \/ (i <= 0)) \/ (i >= 0))) /\ ((((((k <= 0) \/ (k >= 0)) \/ (m <= 1)) \/ (m >= 1)) \/ ~(i <= 0)) \/ ~(i >= 0))) /\ ((((((k <= 0) \/ (k >= 0)) \/ (m <= 1)) \/ (m >= 1)) \/ ~(i <= 0)) \/ (i >= 0))) /\ ((((((k <= 0) \/ (k >= 0)) \/ (m <= 1)) \/ (m >= 1)) \/ (i <= 0)) \/ ~(i >= 0))) /\ ((((((k <= 0) \/ (k >= 0)) \/ (m <= 1)) \/ (m >= 1)) \/ (i <= 0)) \/ (i >= 0))))
//      """.stripMargin
//    println(Simplify(exp))


//    val context:Expression =
//      """
//        |retval2 == 0 /\ n2 == 10 /\ a2 == 0 /\ i2 == 0
//      """.stripMargin
//
//    var r = alpha(context, "(i2 >= 0)(i2 <= 0)")
//    println(r)
//
//    println("----first iter--------------------------------------------------------------------")
//    val q = ExistentialQuantifier(List(Variable("i3")),
//                                   and(r, """two1 == i2 /\ add1 == (two1 + 1) /\ i3 == add1 /\ zero1 == i2 /\ one1 == n2 /\ cmp1 == (zero1 <= one1)"""))
//
//    println(q)
//
//
//    var next = union(r, q, "(i3 >= 0)(i3 <= 0)")
//
//    println(next)
//
//    println("---next iter---------------------------------------------------------------------")
//
//    r = next
//
//    val q1 = ExistentialQuantifier(List(Variable("i3")),
//                                    and(r, """two1 == i2 /\ add1 == (two1 + 1) /\ i3 == add1 /\ zero1 == i2 /\ one1 == n2 /\ cmp1 == (zero1 <= one1)"""))
//
//    println(q1)
//
//    next = union(r, q1, "(i3 >= 0)(i3 <= 0)")
//    println(next)
