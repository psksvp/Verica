package psksvp.Verica


/**
  * Created by psksvp on 11/04/2016.
  */
object Test
{
  import psksvp.Verica.Lang._

  val P = Predicates( Binary(GreaterOrEqual(), Variable("sum"), IntegerValue(0)),
                      Binary(GreaterOrEqual(), Variable("i"), IntegerValue(1)))

  val e = Binary(LessOrEqual(), Variable("i"), Variable("n"))

  val B = Sequence( Assignment(Variable("sum"), Binary(Plus(), Variable("Sum"), Variable("i"))),
                    Assignment(Variable("i"), Binary(Plus(), Variable("i"), IntegerValue(1))))

  val prog = Sequence( Assume(Binary(Greater(), Variable("n"), IntegerValue(0))),
                       Assignment(Variable("sum"), IntegerValue(0)),
                       Assignment(Variable("i"), IntegerValue(0)),
                       While(P, Invariant(True()), e, B),
                       Assert(Binary(Greater(), Variable("sum"), IntegerValue(0)))
                     )


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



    import psksvp.Verica.QuantifierElimination._
    val h = QE(Exists("xp"), SuchThat("""(x = xp - 5) /\ (xp >= 15)"""))
    println(h)
    val g = QE(Exists("ip"), SuchThat("""i = ip + 1 /\ ip >= 0 /\ y2 = ip /\ r = xs + y1 /\ xslen > ip"""))
    println(g)
    println(strongestPostCondition("x := x - 5", "x >= 15")) 

    println(Z3.Validity.check("x + 1 = 2 -> x = 1"))

  }
}
