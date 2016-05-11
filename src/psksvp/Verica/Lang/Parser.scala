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
  lazy val expression: Parser[Expression] = termRelation ~ rep(logicOP ~ termRelation) ^^
  {
    case t ~ ts => ts.foldLeft(t)
    {
      case (t1, "->" ~ t2)     => Binary(Implies(), t1, t2)
      case (t1, """/\""" ~ t2) => Binary(And(), t1, t2)
      case (t1, """\/""" ~ t2) => Binary(Or(), t1, t2)
    }
  }

  lazy val termRelation: Parser[Expression] = termNumericP1 ~ rep(relationOP ~ termNumericP1) ^^
  {
    case t ~ ts => ts.foldLeft(t)
    {
      case (t1, "=" ~ t2)  => Binary(Equal(),t1, t2)
      case (t1, "!=" ~ t2) => Binary(NotEqual(),t1, t2)
      case (t1, ">" ~ t2)  => Binary(Greater(), t1, t2)
      case (t1, ">=" ~ t2) => Binary(GreaterOrEqual(), t1, t2)
      case (t1, "<" ~ t2)  => Binary(Less(), t1, t2)
      case (t1, "<=" ~ t2) => Binary(LessOrEqual(), t1, t2)
    }
  }

  lazy val termNumericP1: Parser[Expression] = termNumericP2 ~ rep(numericP1OP ~ termNumericP2) ^^
  {
    case t ~ ts => ts.foldLeft(t)
    {
      case (t1, "+" ~ t2) => Binary(Plus(), t1, t2)
      case (t1, "-" ~ t2) => Binary(Minus(), t1, t2)
    }
  }

  lazy val termNumericP2: Parser[Expression] = factor ~ rep(numericP2OP ~ factor) ^^
  {
    case t ~ ts => ts.foldLeft(t)
    {
      case (t1, "*" ~ t2) => Binary(Multiply(), t1, t2)
      case (t1, "/" ~ t2) => Binary(Division(), t1, t2)
    }
  }

  lazy val expressionList = repsep(expression, ",")

  lazy val factor = "(" ~> expression <~ ")" | num | trueLiteral | falseLiteral | notExpr | notFunc | variable

  lazy val num = floatingPointNumber                    ^^ {t => IntegerValue(t.toInt) }
  lazy val trueLiteral = "true"                         ^^ {t => True()}
  lazy val falseLiteral = "false"                       ^^ {t => False()}
  lazy val notExpr = "~" ~> expression                  ^^ {t => Unary(Negation(), t)}
  lazy val notFunc = ("Not(" ~> expression <~ ")")      ^^ {t => Unary(Negation(), t)}
  lazy val variable = identifier                        ^^ {t => Variable(t)}

  lazy val numericP2OP = "*" | "/" ^^ {t => t}
  lazy val numericP1OP = "+" | "-" ^^ {t => t}
  lazy val relationOP = "=" | "!=" | "<=" | "<" | ">=" | ">" ^^ {t => t}
  lazy val logicOP = """/\""" | """\/""" | "->" ^^ {t => t}


  lazy val predicates:PackratParser[Predicates] = (("(" ~> expression <~ ")") *) ^^
  {
    case p:Seq[Expression] => psksvp.Verica.Lang.Predicates(p:_*)
  }

  lazy val loopPredicateAndInvariant:PackratParser[PredicatesAndInvariant] = "[" ~ predicates ~ "," ~ expression ~ "]" ^^
  {
    case "[" ~ pred ~ "," ~ inv ~ "]"  => PredicatesAndInvariant(pred, inv)
  }

  /////////////////////////////
  // statement
  lazy val statement:PackratParser[Statement] = assert|assume|whileLoop2|whileLoop|ifElse|assignment|sequence

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

  lazy val whileLoop2:PackratParser[While] = loopPredicateAndInvariant ~ "while(" ~ expression ~ ")" ~ statement ^^
  {
    case pi ~ "while(" ~ exp ~ ")" ~ stm => While(pi.predicates, pi.invariant, exp, stm)
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

  def parseWhile2(src:String):While=
  {
    parseAll(whileLoop2, src) match
    {
      case Success(topNode, _) => topNode
      case f                   => sys.error("expression is not a While expression")
    }
  }

  def parsePreidcate(src:String):Binary=parseBinaryExpression(src)
}

/*
// example
import scala.util.parsing.combinator._

Test.run("""2 + 2 \/ 8 - 3""")

object Test extends ExprParsers2
{
  def run(s:String):Unit=
  {
    println("HelloWorld")
    println(eval(parseAll(expr, s).get))
  }
}

trait ExprParsers2 extends JavaTokenParsers {

  sealed abstract class Tree
  case class Add(t1: Tree, t2: Tree) extends Tree
  case class Sub(t1: Tree, t2: Tree) extends Tree
  case class Mul(t1: Tree, t2: Tree) extends Tree
  case class Div(t1: Tree, t2: Tree) extends Tree
  case class Equal(t1: Tree, t2:Tree) extends Tree
  case class Greater(t1: Tree, t2:Tree) extends Tree
  case class Implies(t1: Tree, t2:Tree) extends Tree
  case class And(t1: Tree, t2: Tree) extends Tree
  case class Or(t1: Tree, t2: Tree) extends Tree
  case class Not(t1: Tree) extends Tree
  case class Num(t: Double) extends Tree
  case class True() extends Tree
  case class False() extends Tree

  def eval(t: Tree): Double = t match
  {
    case Add(t1, t2) => eval(t1)+eval(t2)
    case Sub(t1, t2) => eval(t1)-eval(t2)
    case Mul(t1, t2) => eval(t1)*eval(t2)
    case Div(t1, t2) => eval(t1)/eval(t2)
    case Equal(t1, t2) => 1
    case Implies(t1, t2) => Math.pow(eval(t1), eval(t2))
    case And(t1, t2) => Math.max(eval(t1), eval(t2))
    case Or(t1, t2)  => Math.min(eval(t1), eval(t2))
    case Not(t)      => 0
    case Num(t) => t
    case True() => 1
    case False() => 0
  }

  lazy val expr: Parser[Tree] = termRelation ~ rep(logicOP ~ termRelation) ^^
  {
    case t ~ ts => ts.foldLeft(t)
    {
      case (t1, "->" ~ t2) => Implies(t1, t2)
      case (t1, """/\""" ~ t2) => And(t1, t2)
      case (t1, """\/""" ~ t2) => Or(t1, t2)
    }
  }

  lazy val termRelation: Parser[Tree] = termNumericP1 ~ rep(relationOP ~ termNumericP1) ^^
  {
    case t ~ ts => ts.foldLeft(t)
    {
      case (t1, "=" ~ t2) => Equal(t1, t2)
      case (t1, ">" ~ t2) => Greater(t1, t2)
    }
  }

  lazy val termNumericP1: Parser[Tree] = termNumericP2 ~ rep(numericP1OP ~ termNumericP2) ^^
  {
    case t ~ ts => ts.foldLeft(t)
    {
      case (t1, "+" ~ t2) => Add(t1, t2)
      case (t1, "-" ~ t2) => Sub(t1, t2)
    }
  }

  lazy val termNumericP2 = factor ~ rep(numericP2OP ~ factor) ^^ {
    case t ~ ts => ts.foldLeft(t) {
      case (t1, "*" ~ t2) => Mul(t1, t2)
      case (t1, "/" ~ t2) => Div(t1, t2)
    }
  }

  lazy val factor = "(" ~> expr <~ ")" | num | trueLiteral | falseLiteral | notExpr

  lazy val num = floatingPointNumber ^^ { t => Num(t.toDouble) }
  lazy val trueLiteral = "true" ^^ {t => True()}
  lazy val falseLiteral = "false" ^^ {t => False()}
  lazy val notExpr = "~" ~> expr ^^ {t => Not(t)}


  lazy val numericP2OP = "*" | "/" ^^ {t => t}
  lazy val numericP1OP = "+" | "-" ^^ {t => t}
  lazy val relationOP = "=" | "!=" | "<" | "<=" | ">" | ">=" ^^ {t => t}
  lazy val logicOP = """/\""" | """\/""" | "->" ^^ {t => t}
}
 */


/* does not work
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
    "(" ~> expression <~ ")" */



