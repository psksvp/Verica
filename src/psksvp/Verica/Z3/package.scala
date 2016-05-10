package psksvp.Verica

/**
  * Created by psksvp on 10/05/2016.
  */
package object Z3
{
  import psksvp.Verica.Lang._

  def pythonize(expr:Expression):String=expr match
  {
    case Binary(Or(), l, r)     => "Or(" + pythonize(l) + "," + pythonize(r) + ")"
    case Binary(And(), l, r)    => "And(" + pythonize(l) + "," + pythonize(r) + ")"
    case Binary(Equal(), l, r)  =>  pythonize(l) + "==" + pythonize(r)
    case Binary(Implies(), l, r)=> "Implies(" + pythonize(l) + "," + pythonize(r) + ")"
    case Unary(Negation(), l)   => "Not(" + pythonize(l) + ")"
    case _                      => expr.toString
  }


  def makeIntVariable(v:Variable):String = s"${v.name} = Int('${v.name}')"
  def makeIntVariables(expr:Expression):String =
  {
    var result = ""
    for(v <- psksvp.removeDuplicate(listOfVariablesIn(expr)))
      result = result.concat(makeIntVariable(v) + "\n")

    result.trim
  }
}
