package psksvp.Verica.Lang

import scala.util.parsing.combinator._
/**
  * Created by psksvp on 20/04/2016.
  */
object Parser extends JavaTokenParsers with PackratParsers
{
  lazy val variable:PackratParser[Variable] = ident ^^
  {
    case id => Variable(id.toString)
  }

  lazy val integer:PackratParser[IntegerValue] = decimalNumber ^^
  {
    case x => IntegerValue(x.toInt)
  }

  lazy val bool:PackratParser[Value[Boolean]] = ("true" | "false") ^^
  {
    case "true" => True()
    case "false" => False()
  }

  lazy val unaryOperator:PackratParser[Operator] = ("+"|"-"|"¬") ^^
  {
    case "+"  => Plus()
    case "-"  => Minus()
    case "¬"  => Negation()
  }

  lazy val binaryOperator:PackratParser[Operator] = ("+"|"-"|"*"|"/"|"=="|
                                                     ">"|"<"|">="|"<="|"!="|
                                                     "⋁"|"⋀") ^^
  {
    case "+"  => Plus()
    case "-"  => Minus()
    case "*"  => Multiply()
    case "/"  => Division()
    case "==" => Equal()
    case ">"  => Greater()
    case "<"  => Less()
    case ">=" => GreaterOrEqual()
    case "<=" => LessOrEqual()
    case "!=" => NotEqual()
    case "⋁"  => Or()
    case "⋀"  => And()
  }

  /////////////////
  // expression
  lazy val unary:PackratParser[Unary] = (unaryOperator ~ expression) ^^
  {
    case op ~ exp => Unary(op, exp)
  }

  lazy val binary:PackratParser[Binary] = expression ~ binaryOperator ~ expression ^^
  {
    case exp1 ~ op ~ exp2 => Binary(op, exp1, exp2)
  }

  lazy val invariant:PackratParser[Invariant] = expression ^^
  {
    case exp => Invariant(exp)
  }

  lazy val Predicates:PackratParser[Predicates] = (("(" ~> expression <~ ")") *) ^^
  {
    case p:Seq[Expression] => psksvp.Verica.Lang.Predicates(p:_*)
  }

  lazy val expression:PackratParser[Expression] = variable|integer|bool|unary|binary

  /////////////////////////////
  // statement
  lazy val statement:PackratParser[Statement] = assignment|assert|assume|sequence|whileLoop

  lazy val assignment:PackratParser[Assignment] = (variable ~ ":=" ~ expression) ^^
  {
    case v ~ ":=" ~ exp => Assignment(v, exp)
  }

  lazy val assert:PackratParser[Assert] = ("assert" ~ "(" ~expression~")") ^^
  {
    case "assert" ~"(" ~ exp ~ ")" => Assert(exp)
  }

  lazy val assume:PackratParser[Assert] = ("assume" ~ "(" ~expression~")") ^^
  {
    case "assume" ~"(" ~ exp ~ ")" => Assert(exp)
  }

  lazy val sequence:PackratParser[Sequence] = ("begin" ~> (statement *) <~ "end") ^^
  {
    case s:Seq[Statement] => Sequence(s: _*)
  }

  lazy val whileLoop:PackratParser[While] = (("while(" ~> expression <~ ")") ~ statement) ^^
  {
    case exp ~ stm => While(null, null, exp, stm)
  }

  lazy val module:PackratParser[Module] = (("module(" ~> ident <~ ")") ~ sequence) ^^
  {
    case name ~ seqq => Module(ident.toString, seqq)
  }

  def parse(src:String):Module=
  {
    parseAll(module, src) match
    {
      case Success(topNode, _) => topNode.asInstanceOf[Module]
      case f =>
        sys.error("error while parsing: " + f)
    }
  }
}
