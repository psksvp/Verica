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
      case (t1, "==" ~t2)  => Binary(Equal(),t1, t2)
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

  lazy val factor = "(" ~> expression <~ ")" |num|trueLiteral|falseLiteral|notExpr|notFunc|andFunc|orFunc|variable

  lazy val num = floatingPointNumber                    ^^ {t => IntegerValue(t.toInt) }
  lazy val trueLiteral = "true"                         ^^ {t => True()}
  lazy val falseLiteral = "false"                       ^^ {t => False()}
  lazy val notExpr = "~" ~> expression                  ^^ {t => Unary(Negation(), t)}
  lazy val notFunc = "Not(" ~> expression <~ ")"        ^^ {t => Unary(Negation(), t)}
  lazy val andFunc = "And(" ~> expressionList <~ ")"    ^^ {t => psksvp.Verica.and(t)}
  lazy val orFunc = "Or(" ~> expressionList <~ ")"      ^^ {t => psksvp.Verica.or(t)}
  lazy val variable = identifier                        ^^ {t => Variable(t)}

  lazy val numericP2OP = "*" | "/" ^^ {t => t}
  lazy val numericP1OP = "+" | "-" ^^ {t => t}
  lazy val relationOP = "==" | "=" | "!=" | "<=" | "<" | ">=" | ">" ^^ {t => t}
  lazy val logicOP = """/\""" | """\/""" | "->" ^^ {t => t}


  lazy val predicates:PackratParser[Predicates] = (("(" ~> expression <~ ")") *) ^^
  {
    case p:Seq[Expression] => psksvp.Verica.Lang.Predicates(p:_*)
  }

  lazy val loopPredicateAndInvariant:PackratParser[PredicatesAndInvariant] = "[" ~ predicates ~ "," ~ expression ~ "]" ^^
  {
    case "[" ~ pred ~ "," ~ inv ~ "]"  => PredicatesAndInvariant(pred, inv)
  }

  lazy val z3pyListOutput:PackratParser[List[Expression]] = "[[" ~ expressionList ~ "]]" ^^
  {
    case "[[" ~ exprLs ~ "]]" => exprLs
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
      case f                   => sys.error("expression is not a While2 expression")
    }
  }

  def parseZ3ListOutput(src:String):List[Expression]=
  {
    parseAll(z3pyListOutput, src) match
    {
      case Success(topNode, _) => topNode
      case f                   => sys.error("expression is not a z3pyListOutput expression")
    }
  }

  def parsePreidcate(src:String):Binary=parseBinaryExpression(src)
}


