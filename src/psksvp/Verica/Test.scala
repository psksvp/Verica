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

    //val m = Parser.parse(prog1)

    Parser.parseExpression("1 + 2")
    //println( norm(True(), "{ assume(i=0)  assume(r=0)  }"))

    /*
    println(m)
    val p = makePredicate("(x == 0)")
    val q = makePredicate("(x == 1)")
    println(p)
    println(q)
    println(vc(p, m.sequence, q))
    //println(Lispified(wp(m.sequence, p)))

    println( norm(True(), "{ assume(i=0)  assume(r=0)  }")) */
    //import psksvp.Verica.QuantifierElimination._
    //val h = Solve(Exists(Variable("x"), Variable("xp")), SuchThat("""((x = xp - 5) && (xp > 15))"""))
  }
}
