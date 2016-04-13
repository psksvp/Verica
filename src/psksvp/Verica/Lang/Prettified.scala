package psksvp.Verica.Lang

/**
  * Created by psksvp on 13/04/2016.
  */
object Prettified
{
  def apply(n:Node):String =
  {

    n match
    {
      case s:Statement if false == s.isInstanceOf[Sequence] => indentString + pretty(n)
      case _                                                => pretty(n)
    }
  }

  def pretty(n:Node):String =
  {
    n match
    {
      case op:Operator      => op.symbol
      case v:Value[_]       => v.value.toString
      case Variable(v)      => v
      case Literal(l)       => l
      case Assert(e)        => s"Assert(${apply(e)})"
      case Assume(e)        => s"Assume(${apply(e)})"
      case Binary(op, l, r) => s"${apply(l)} ${apply(op)} ${apply(r)}"
      case Unary(op, opd)   => s"${apply(op)}${apply(opd)}"
      case Assignment(v, e) => s"${apply(v)} := ${apply(e)}"
      case Choice(a, b)     => s"${apply(a)} ☐ ${apply(b)}"
      case Invariant(e)     => apply(e)
      case s:Sequence       => pretty(s)
      case p:Predicates     => pretty(p)
      case While(p, i, e, s)=> indent()
                               val out = s"{${apply(p)}, ${apply(i)}} while(${apply(e)})\n${apply(s)}"
                               outdent()
                               out

    }
  }

  def pretty(seq:Sequence): String =
  {
    var out = ""
    for(s <- seq.stmts)
    {
      out = out.concat(s"${apply(s)}\n")
    }
    out
  }

  def pretty(p:Predicates):String=
  {
    var s = ""
    for(e <- p.exprs)
    {
      s = s.concat(s"(${apply(e)})")
    }
    s
  }


  private val indentSize = 2
  private var nIndent = 0
  def indent():Unit = nIndent = nIndent + indentSize
  def outdent():Unit = nIndent = nIndent - indentSize
  def indentString = " " * nIndent
}
