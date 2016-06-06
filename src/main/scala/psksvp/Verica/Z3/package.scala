package psksvp.Verica

/**
  * Created by psksvp on 10/05/2016.
  */
package object Z3
{
  import psksvp.Verica.Lang._

  /**
    * Verica language to python string
    *
    * @param expr
    * @return
    */
  def pythonize(expr:Expression):String=expr match
  {
    case Binary(Or(), l, r)          => "Or(" + pythonize(l) + "," + pythonize(r) + ")"
    case Binary(And(), l, r)         => "And(" + pythonize(l) + "," + pythonize(r) + ")"
    case Binary(Equal(), l, r)       =>  pythonize(l) + "==" + pythonize(r)
    case Binary(Implies(), l, r)     => "Implies(" + pythonize(l) + "," + pythonize(r) + ")"
    case Binary(op, l, r)            => pythonize(l) + op.symbol + pythonize(r)
    case Unary(Negation(), l)        => "Not(" + pythonize(l) + ")"
    case Length(v)                   => s"lengthOf_${v.name}"
    case ExistentialQuantifier(v, e) => s"Exists(${Prettified.pretty(v.toList)}, ${pythonize(e)})"
    case UniversalQuantifier(v, e)   => s"ForAll(${Prettified.pretty(v.toList)}, ${pythonize(e)})"
    case True()                      => "True"
    case False()                     => "False"
    case _                           => expr.toString
  }


  /**
    *
    * @param v
    * @return
    */
  def makeIntVariable(v:Variable):String = v match
  {
    case Variable(n, Nil, ValueVariable()) => s"$n = Int('$n')"
    case Variable(n, _, ArrayVariable())   => s"$n = Array('$n', IntSort(), IntSort())"
  }


  /**
    *
    * @param s
    * @return
    */
  def makeIntVariables(s:Seq[Variable]):String =
  {
    var decl = ""
    for(v <- s)
    {
      decl = decl + makeIntVariable(v) + "\n"
    }
    decl.trim
  }
  def makeIntVariables(expr:Expression):String =
  {
    var result = ""
    for(v <- psksvp.removeDuplicate(listOfVariablesIn(expr)))
      result = result.concat(makeIntVariable(v) + "\n")

    result.trim
  }

  def makeAssumptions(modelName:String, ls:List[Expression]):String = ls match
  {
    case Nil => ""
    case h :: rest => makeAssumption(modelName, h) + makeAssumptions(modelName, rest)
  }

  def makeAssumption(modelName:String, expr:Expression):String =
  {
    expr match
    {
      case UniversalQuantifier(v, e) => makeIntVariables(v) + "\n" +
                                        makeIntVariables(e) + "\n" +
                                        s"$modelName.add(${pythonize(expr)})\n"
      case _                         => s"$modelName.add(${pythonize(expr)})\n"
    }

  }

}
