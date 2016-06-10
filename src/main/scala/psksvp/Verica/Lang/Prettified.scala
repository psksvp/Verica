package psksvp.Verica.Lang

import javax.swing.JPopupMenu.Separator

/**
  * Created by psksvp on 13/04/2016.
  */
object Prettified
{
  def apply(n:Node):String = n match
  {
    case s:Statement if false == s.isInstanceOf[Sequence] => indentString + pretty(n)
    case _                                                => pretty(n)
  }

  def pretty(n:Node):String = n match
  {
    case op:Operator                      => op.symbol
    case v:Value[_]                       => v.value.toString
    case Variable(v, _, ValueVariable())  => v
    case Variable(v, Nil, ArrayVariable())=> v
    case Variable(v, il, ArrayVariable()) => v + "[" + pretty(il) + "]"
    case Length(v)                        => v + ".length"
    case Literal(l)                       => l
    case i:TypeClass                      => i.toString
    case UniversalQuantifier(v, e)        => s"forAll(${pretty(v.toList)}, ${apply(e)})"
    case ExistentialQuantifier(v, e)      => s"exists(${pretty(v.toList)}, ${apply(e)})"
    case v:VerificationStatment           => s"${v.name}(${apply(v.expression)})"
    case VariableDeclaration(name, t)     => s"local $name:${apply(t)}"
    case InvokeExpression(m, f, args)     => s"$m.$f(" + pretty(args) + ")"
    case InvokeStatement(m, f, args)      => s"$m.$f(" + pretty(args) + ")"
    case Return(e)                        => s"return(${apply(e)})"
    case Binary(op, l, r)                 => s"(${apply(l)} ${apply(op)} ${apply(r)})"
    case Unary(op, opd)                   => s"${apply(op)}${apply(opd)}"
    case Assignment(v, e)                 => s"${apply(v)} := ${apply(e)}"
    case Choice(a, b)                     => s"${apply(a)} ☐ ${apply(b)}"
    case s:Sequence                       => pretty(s)
    case p:Predicates                     => pretty(p)
    case While(p, i, e, s)                => s"while(${apply(e)}, [${apply(p)}, ${apply(i)}])\n${apply(s)}"
    case Parameter(name, t)               => s"$name:$t"
    case Function(f, args, tpe, body, vl) => s"function $f(${pretty(args)}):${apply(tpe)}\n[${pretty(vl)}]\n${apply(body)}"
    case a:If                             => pretty(a)
    case Empty()                          => ""
    case Module(name, f)                  => val h = s"Module($name)\n{\n"
                                             indent()
                                             val i = indentString + s"${pretty(f, "  ")}"
                                             outdent()
                                             h + i + "\n}"
  }


  def pretty[T <: Node](il:List[T], separator: String = ", "):String = il match
  {
    case Nil       => ""
    case e :: Nil  => apply(e)
    case e :: rest => apply(e) + separator + pretty[T](rest, separator)
  }


  def pretty(iF:If): String =
  {
    var out = "if(" + apply(iF.e) + ")\n"
    out = out + apply(iF.bodyA)
    if(iF.bodyB != Empty())
    {
      out = out + "\n" + indentString + "else\n"
      out = out + apply(iF.bodyB)
    }

    out
  }

  def pretty(seq:Sequence): String =
  {
    val openCurly = indentString + "{\n"
    indent()
    val body = pretty(seq.stmts.toList, "\n")
    outdent()
    val closeCurly = "\n" + indentString + "}\n"

    s"$openCurly$body$closeCurly"
  }

  def pretty(p:Predicates):String=pretty(p.exprs.toList, "")


  private val indentSize = 2
  private var nIndent = 0
  def indent():Unit = nIndent = nIndent + indentSize
  def outdent():Unit = nIndent = nIndent - indentSize
  def indentString = " " * nIndent
}
