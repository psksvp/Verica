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
    case Binary(Equal(), l, r)       =>  "(" + pythonize(l) + ")" + "==" + "(" + pythonize(r) + ")"
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

  def pythonize(exprs:List[Expression]):String =
  {
    def go(exprs: List[Expression]): String = exprs match
    {
      case Nil       => ""
      case h :: Nil  => pythonize(h)
      case h :: rest => pythonize(h) + ", " + pythonize(rest)
    }
    go(exprs)
  }

  def pythonize(stm:Statement):String = stm match
  {
    case Assignment(Variable(n, i :: r, ArrayVariable()), e) => s"n = Store(n, ${pythonize(i)}, ${pythonize(e)})"
  }


  def makeVariable(v:Variable):String =
  {
    Register.lookup(v) match
    {
      case Some(varDecl) => varDecl.typeClass match
                            {
                              case IntegerType() => makeIntVariable(v)
                              case BooleanType() => makeBoolVariable(v)
                            }
      case _             => //println(s"z3 default variable ${v.name} to Int")
                            makeIntVariable(v)
    }
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
    * @param v
    * @return
    */
  def makeBoolVariable(v:Variable):String = v match
  {
    case Variable(n, Nil, ValueVariable()) => s"$n = Bool('$n')"
    case Variable(n, _, ArrayVariable())   => s"$n = Array('$n', Bool(), Bool())"
  }

  /**
    *
    * @param s
    * @return
    */
  def makeVariables(s:Seq[Variable]):String =
  {
    val decl = for(v <- s) yield makeVariable(v) + "\n"
    decl.reduce(_ + _).trim
  }

  def makeVariables(expr:Expression):String =
  {
    val result = for(v <- psksvp.removeDuplicate(listOfVariablesIn(expr))) yield makeVariable(v) + "\n"
    result.reduce(_ + _).trim
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
      case UniversalQuantifier(v, e) => makeVariables(v) + "\n" +
                                        makeVariables(e) + "\n" +
                                        s"$modelName.add(${pythonize(expr)})\n"
      case _                         => s"$modelName.add(${pythonize(expr)})\n"
    }

  }

}
