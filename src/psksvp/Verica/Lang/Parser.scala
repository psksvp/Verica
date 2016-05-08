package psksvp.Verica.Lang

import scala.util.parsing.combinator._
/**
  * Created by psksvp on 20/04/2016.
  */
object Parser extends JavaTokenParsers with PackratParsers
{
  lazy val identifier = regex("[a-zA-Z][a-zA-Z0-9_]*".r)
  lazy val integer = regex("[0-9]+".r)

  /////////////////
  // expression

  lazy val expression:PackratParser[Expression] =
    expression ~ ("&" ~> expression2) ^^  { case l ~ r => Binary(And(), l, r)}   |
    expression ~ ("|" ~> expression2) ^^  { case l ~ r => Binary(Or(), l, r)}    |
    expression ~ ("->" ~> expression2) ^^ { case l ~ r => Binary(Implies(), l, r)} |  expression2

  lazy val expression2:PackratParser[Expression] =
    expression1 ~ ("!=" ~> expression1) ^^ {case l ~ r => Binary(NotEqual(), l, r)} |
    expression1 ~ ("=" ~> expression1) ^^ {case l ~ r => Binary(Equal(), l, r)} |
    expression1 ~ ("<" ~> expression1) ^^ {case l ~ r => Binary(Less(), l, r)} |
    expression1 ~ ("<=" ~> expression1) ^^ {case l ~ r => Binary(LessOrEqual(), l, r)} |
    expression1 ~ (">" ~> expression1) ^^ {case l ~ r => Binary(Greater(), l, r)} |
    expression1 ~ (">=" ~> expression1) ^^ {case l ~ r => Binary(GreaterOrEqual(), l, r)} |expression1

  lazy val expression1:PackratParser[Expression] =
    expression1 ~ ("+" ~> expression0) ^^ {case l ~ r => Binary(Plus(), l, r)} |
    expression1 ~ ("-" ~> expression0) ^^ {case l ~ r => Binary(Minus(), l, r)} | expression0

  lazy val expression0:PackratParser[Expression] =
    expression0 ~ ("/" ~> factor) ^^ {case l ~ r => Binary(Division(), l, r)}
    expression0 ~ ("*" ~> factor) ^^ {case l ~ r => Binary(Multiply(), l, r)} | factor

  lazy val factor:PackratParser[Expression] =
    integer           ^^ {case s  => IntegerValue(s.toInt)} |
    "true"            ^^ {case _  => True()}                |
    "false"           ^^ {case _  => False()}               |
    identifier        ^^ {case id => Variable(id)}          |
    "~" ~> expression ^^ {case e  => Unary(Negation(), e)}  |
    "(" ~> expression <~ ")"



  lazy val invariant:PackratParser[Invariant] = expression ^^
  {
    case exp => Invariant(exp)
  }

  lazy val Predicates:PackratParser[Predicates] = (("(" ~> expression <~ ")") *) ^^
  {
    case p:Seq[Expression] => psksvp.Verica.Lang.Predicates(p:_*)
  }

  /////////////////////////////
  // statement
  lazy val statement:PackratParser[Statement] = assert|assume|whileLoop|ifElse|assignment|sequence

  lazy val assignment:PackratParser[Assignment] = identifier ~ (":=" ~> expression) ^^
  {
    case v ~ exp => Assignment(Variable(v), exp)
  }

  lazy val assert:PackratParser[Assert] = "assert" ~> ("(" ~> expression <~ ")") ^^
  {
    case exp  => Assert(exp)
  }

  lazy val assume:PackratParser[Assume] = "assume" ~> ("(" ~> expression <~ ")") ^^
  {
    case exp  => Assume(exp)
  }

  lazy val sequence:PackratParser[Sequence] = ("{" ~> (statement *) <~ "}") ^^
  {
    case s:Seq[Statement] => Sequence(s: _*)
  }

  lazy val whileLoop:PackratParser[While] = (("while(" ~> expression <~ ")") ~ statement) ^^
  {
    case exp ~ stm => While(null, null, exp, stm)
  }


  lazy val ifElse:PackratParser[If] = "if" ~> ("(" ~> expression <~ ")") ~ statement ~ (("else" ~> statement)?) ^^
  {
    case expr ~ stmTrue ~ stmFalseOption =>
      stmFalseOption match
      {
        case Some(stmFalse) => If(expr, stmTrue, stmFalse)
        case None => If(expr, stmTrue)
      }
  }

  lazy val module:PackratParser[Module] = (("module(" ~> identifier <~ ")") ~ sequence) ^^
  {
    case name ~ seqq => Module(name, seqq)
  }



  //////////////////////////////////////////////////
  def parse(src:String):Module=
  {
    parseAll(module, src) match
    {
      case Success(topNode, _) => topNode //.asInstanceOf[Module]
      case f                   => sys.error("error while parsing: " + f)
    }
  }

  def parseStatement(src:String):Statement=
  {
    parseAll(statement, src) match
    {
      case Success(topNode, _) => topNode
      case f                   => sys.error("error while parsing: " + f)
    }
  }

  def parseExpression(src:String):Expression=
  {
    parseAll(expression, src) match
    {
      case Success(topNode, _) => topNode
      case f                   => sys.error("error while parsing: " + f)
    }
  }

  def parseBinaryExpression(src:String):Binary=
  {
    parseExpression(src) match
    {
      case b:Binary => b
      case _        => sys.error("expression is not a Binary expression")
    }
  }

  def parsePreidcate(src:String):Binary=parseBinaryExpression(src)
}
