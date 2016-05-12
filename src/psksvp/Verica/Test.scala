package psksvp.Verica

import psksvp.Verica.Z3.{Exists, QE, SuchThat, Validity}


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

    val a:AbstractDomain = List(Array(true, false, true), Array(true, true, true), Array(true, false, false))
    val expr:Expression = a
    println(expr)

    println("---------")
    val aa:AbstractDomain = List(Array(true, false), Array(true, true), Array(true, false))
    println(gamma(aa, Predicates("i>=0", "r >= 0")))
    */

    val m =
      """
        |module(bababa)
        |{
        |  assume(n > 0)
        |  i := 0
        |  s := 0
        |  [(i >= 0)(s >= 0), true]while(i < n)
        |  {
        |    s := s + i
        |    i := i + 1
        |  }
        |}
      """.stripMargin

    val wh = Parser.parse(m)
    println(wh)
    traverse(wh.body)

    /*
    val lsex = Parser.parseZ3ListOutput("[[Not(n <= 0), i == 0, i >= 23, And(i > 5, j < 10, 4 = 5), Or(2 > 3, 3 < 4)]]")
    println(lsex)
    println(and(lsex))*/
  }
}
