package psksvp.Verica

import psksvp.Verica.Z3.{Exists, QE, SuchThat, Validity}
import psksvp.Verica.PredicateAbstractionForSoftwareVerification._


/**
  * Created by psksvp on 11/04/2016.
  */
object Test
{
  import psksvp.Verica.Lang._


  def main(args:Array[String]):Unit=
  {
    val prog9 =
      """
        |module(main)
        |{
        |  if(x != 0)
        |    z := x
        |  else
        |    z := x + 1
        |}
      """.stripMargin

    val prog1 =
      """
        |module(main)
        |{
        |  x := x + 1
        |}
      """.stripMargin




    /*
    val h = QE.solve(Exists("xp"), SuchThat("""(x = xp - 5) /\ (xp >= 15)"""))
    println(h)
    val g = QE.solve(Exists("ip"), SuchThat("""i = ip + 1 /\ ip >= 0 /\ y2 = ip /\ r = xs + y1 /\ xslen > ip"""))
    println(g)
    println(strongestPostCondition("x := x - 5", "x >= 15"))

    println(Validity.check("x + 1 = 2 -> x = 1"))


    val r = alpha("""i = 0 /\ r = 0""", Predicates("i>=0", "r >= 0"))
    println(r + "")
    println(gamma(r, Predicates("i>=0", "r >= 0")))

    val a:AbstractDomain = List(Vector(true, false, true), Vector(true, true, true), Vector(true, false, false))
    val expr:Expression = a
    println(expr)

    println("---------")
    val aa:AbstractDomain = List(Vector(true, false), Vector(true, true), Vector(true, false))
    println(gamma(aa, Predicates("i>=0", "r >= 0")))
    */

    val m =
      """
        |module(bababa)
        |{
        |  assume(n > 0)
        |  i := 1
        |  s := 0
        |  while(i <= n, [(i >= 1)(i = s + 1)(i<1)(s>=0), true])
        |  {
        |    s := s + i
        |    i := i + 1
        |  }
        |}
      """.stripMargin

    val wh = Parser.parse(m)
    println(wh)
    println(traverse(wh.body))

    awp("{R := R + Y  Q := Q + 1}", "x == R + Y * Q")

    val prg:Statement=
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
    for(f <- kk)
      println(f + " is " + Validity.check(f))


    println(strongestPostCondition("{x:=0 y:=0}", True()))

    val mm:Expression = "a + b[i * 2, j + 1] = a + x[b.length]"
    println(mm)
    println(Z3.pythonize(mm))
    val vari = Variable("h", List("1"), ArrayVariable())
    println(Z3.makeIntVariable(vari))
  }
}
