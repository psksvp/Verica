package psksvp.Verica

/**
  * Created by psksvp on 15/07/2016.
  *  A + AB = A
  *  A + ~AB = A + B
  *  (A + B)(A + C) = A + BC
  */
object Simplify
{
  import psksvp.Verica.Lang._

  def apply(exp:Expression):Expression = exp match
  {
    //very very ugly
    case Binary(Or(), True(), a) => True()
    case Binary(Or(), a, True()) => True()
    case Binary(And(), False(), a) => False()
    case Binary(And(), a, False()) => False()
    case Binary(Or(), a, Binary(And(), ap, b)) if a == ap => Simplify(a)
    case Binary(Or(), a, Binary(And(), Unary(Negation(), ap), b)) if a == ap => Binary(Or(), Simplify(a), Simplify(b))
    case Binary(And(), Binary(Or(), a, b),
                       Binary(Or(), ap, c)) if a == ap => Binary(Or(), Simplify(a),
                                                                       Binary(And(), Simplify(b), Simplify(c)))

    case _ => exp
  }
}
