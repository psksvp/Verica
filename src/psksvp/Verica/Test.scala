package psksvp.Verica


/**
  * Created by psksvp on 11/04/2016.
  */
object Test
{
  import psksvp.Verica.Lang._

  val P = Predicates( Binary(GreaterOrEqual(), Variable("sum"), Literal("0")),
                      Binary(GreaterOrEqual(), Variable("i"), Literal("1")))

  val e = Binary(LessOrEqual(), Variable("i"), Variable("n"))

  val B = Sequence( Assignment(Variable("sum"), Binary(Plus(), Variable("Sum"), Variable("i"))),
                    Assignment(Variable("i"), Binary(Plus(), Variable("i"), Literal("1"))))

  val prog = Sequence( Assume(Binary(Greater(), Variable("n"), Literal("0"))),
                       Assignment(Variable("sum"), Literal("0")),
                       Assignment(Variable("i"), Literal("0")),
                       While(P, Invariant(True()), e, B),
                       Assert(Binary(Greater(), Variable("sum"), Literal("0")))
                     )


  def main(args:Array[String]):Unit=
  {
    println(prog)
    traverse(Empty(), prog)
  }
}
