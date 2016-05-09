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

    //println(Parser.parse(prog9))
    //println(Parser.parse(prog1))

    //println(Parser.parseExpression("""2 + 2 \/ 8 - 3"""))
    //println( norm(True(), "{ assume(i >= -10)  assume(r=0)  }"))

    /*
    println(m)
    val p = makePredicate("(x == 0)")
    val q = makePredicate("(x == 1)")
    println(p)
    println(q)
    println(vc(p, m.sequence, q))
    //println(Lispified(wp(m.sequence, p)))

    println( norm(True(), "{ assume(i=0)  assume(r=0)  }")) */
    import psksvp.Verica.QuantifierElimination._
    val h = QE(Exists("xp"), SuchThat("""(x = xp - 5) /\ (xp > 15)"""))
    println(h)
    val g = QE(Exists("ip"), SuchThat("""i = ip + 1 /\ ip >= 0 /\ y2 = ip /\ r = xs + y1 /\ xslen > ip"""))
    println(g)

    //println(Parser.parseExpression("""x = xp - 5 /\ Not(xp) > 15"""))
    //println(z3Pythonize("""x = xp - 5 /\ Not(xp) > 15  /\ x = 1"""))
  }
}
