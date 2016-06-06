package psksvp.Verica

import psksvp.Verica.Z3.{Exists, QE, SuchThat, Validity}
import psksvp.Verica.PredicateAbstractionForSoftwareVerification._



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
      |    local i:Integer
      |    local s:Integer
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
      |    local i:Integer
      |    local s:Integer
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
      |    local i:Integer
      |    local s:Integer
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
        |  [assume(a.length >= 0),
        |   assume(forAll(j, a[j] >= 0)),
        |   ensure(r >= 0)]
        |  {
        |    local i:Integer
        |    local r:Integer
        |    i := 0
        |    r := 0
        |    while(i < a.length, [(r >= 0)(i >= 0)(r < 0)(i < 0), true ])
        |    {
        |      r := r + a[i]
        |      i := i + 1
        |    }
        |  }
      """.stripMargin



    println(f1)
    val f2 = traverse(f1)
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
        |   local i:Integer
        |   local result:Integer
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


  def main(args:Array[String]):Unit=
  {
    testInferWithArray
  }
}
