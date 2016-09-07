package psksvp.Verica



/**
  * Created by psksvp on 7/09/2016.
  */
package object SymPy
{
  import psksvp.Verica.Lang._
  def pythonize(expr:Expression):String=expr match
  {
    case Binary(Or(), l, r)          => "(" + pythonize(l) + " | " + pythonize(r) + ")"
    case Binary(And(), l, r)         => "(" + pythonize(l) + " & " + pythonize(r) + ")"
    case Binary(Equal(), l, r)       =>  "(" + pythonize(l) + ")" + " == " + "(" + pythonize(r) + ")"
    case Binary(Implies(), l, r)     => "(" + pythonize(l) + " >> " + pythonize(r) + ")"
    case Unary(Negation(), l)        => "~" + pythonize(l)
    case True()                      => "True"
    case False()                     => "False"
    case Variable(n, _, _)           => n
    case _                           => sys.error("SymPy.pythonize expression might contain non-bool variables")
  }

  def symplify(expr:Expression):Expression =
  {
    def varsToSymbol(lsv:List[Variable]):String = lsv match
    {
      case Nil => ""
      case h :: rest => s"${h.name} = symbols('${h.name}')\n" + varsToSymbol(rest)
    }


    val vars = psksvp.removeDuplicate(listOfVariablesIn(expr))

    val code =
      s"""
        |from sympy import *
        |${varsToSymbol(vars)}
        |print(simplify_logic( ${pythonize(expr)} ))
      """.stripMargin

    psksvp.evalPython(code, "/usr/local/bin/python")
  }
}
