package psksvp.Verica.Lang

/**
  * Created by psksvp on 20/04/2016.
  */
object Lispified
{
  def apply(n:Node):String = n match
  {
    case op:Operator             => op.symbol
    case v:Value[_]              => v.value.toString
    case Variable(v)             => v
    case Literal(l)              => l
    case Assert(e)               => s"(Assert ${apply(e)})"
    case Assume(e)               => s"(Assume ${apply(e)})"
    case Binary(op, l, r)        => s"(${apply(op)} ${apply(l)} ${apply(r)})"
    case Unary(op, opd)          => s"(${apply(op)} ${apply(opd)}"
    case Assignment(v, e)        => s"(:= ${apply(v)} ${apply(e)})"
    case Choice(a, b)            => s"(☐ ${apply(a)} ${apply(b)})"
    case Invariant(e)            => apply(e)
    case Sequence(s, rest@ _*)   => s"${apply(s)}\n" + apply(Sequence(rest:_*))
    case Predicates(p, rest@ _*) => s"${apply(p)}" + apply(Predicates(rest:_*))
    case While(p, i, e, s)       => s"(while {${apply(p)}, ${apply(i)}} ${apply(e)}) ${apply(s)})"
    case _                       => ""
  }
}
