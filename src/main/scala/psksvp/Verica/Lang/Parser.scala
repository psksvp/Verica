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

  lazy val factor = "(" ~> expression <~ ")" |num|trueLiteral|falseLiteral|notExpr|notFunc|andFunc|orFunc|
                                              arrayVariable|variableLength|invokeExp|z3pyLengthOfArray|
                                              forAll|exists|z3pySysGenVar|variable

  lazy val num = floatingPointNumber                    ^^ {t => IntegerValue(t.toInt) }
  lazy val trueLiteral = "true"                         ^^ {t => True()}
  lazy val falseLiteral = "false"                       ^^ {t => False()}
  lazy val notExpr = "~" ~> expression                  ^^ {t => Unary(Negation(), t)}
  lazy val notFunc = "Not(" ~> expression <~ ")"        ^^ {t => Unary(Negation(), t)}
  lazy val andFunc = "And(" ~> expressionList <~ ")"    ^^ {t => psksvp.Verica.and(t)}
  lazy val orFunc = "Or(" ~> expressionList <~ ")"      ^^ {t => psksvp.Verica.or(t)}
  lazy val variable = identifier                        ^^ {t => Variable(t)}
  lazy val invokeExp = identifier ~ "." ~ identifier ~ ("(" ~> expressionList <~ ")")  ^^
  {
    case modName ~ _ ~ funcName ~ args => InvokeExpression(modName, funcName, args)
  }

  lazy val arrayVariable = variable ~ ("[" ~> expressionList <~ "]") ^^  // 1d array only at the moment
  {
    case v ~ indices => Variable(v.name, indices, ArrayVariable())
  }

  lazy val variableLength = variable <~ "." <~ "length" ^^ {t => Length(t)}

  lazy val numericP2OP = "*" | "/" ^^ {t => t}
  lazy val numericP1OP = "+" | "-" ^^ {t => t}
  lazy val relationOP = "==" | "=" | "!=" | "<=" | "<" | ">=" | ">" ^^ {t => t}
  lazy val logicOP = """/\""" | """\/""" | "->" ^^ {t => t}

  lazy val universalQuantifierName = "forAll"|"ForAll"
  lazy val existentialQuantifierName = "exists"|"Exists"

  lazy val forAll:PackratParser[UniversalQuantifier] =
    universalQuantifierName ~> ("(" ~> repsep(z3pySysGenVar|variable, " ")) ~ ("," ~> expression <~ ")") ^^
  {
    case v ~ expr => UniversalQuantifier(v, expr)
  }

  lazy val exists:PackratParser[ExistentialQuantifier] =
    existentialQuantifierName ~> ("(" ~> repsep(z3pySysGenVar|variable, " ")) ~ ("," ~> expression <~ ")") ^^
  {
    case v ~ expr => ExistentialQuantifier(v, expr)
  }
  ///////////////////////////////
  // predicates and Invariant
  lazy val predicates:PackratParser[Predicates] = (("(" ~> expression <~ ")") *) ^^
  {
    case p:Seq[Expression] => psksvp.Verica.Lang.Predicates(p:_*)
  }

  lazy val loopPredicateAndInvariant:PackratParser[PredicatesAndInvariant] = "[" ~ predicates ~ "," ~ expression ~ "]" ^^
  {
    case "[" ~ pred ~ "," ~ inv ~ "]"  => PredicatesAndInvariant(pred, inv)
  }


  //////////////////////////////////
  /// z3python output
  lazy val z3pySysGenVar:PackratParser[Variable] = identifier~"!"~integer ^^
  {
    case s~"!"~n => Variable(s"$s$n")
  }
  lazy val z3pyLengthOfArray:PackratParser[Length] = "lengthOf_" ~> identifier ^^
  {
    t => Length(Variable(t, Nil, ArrayVariable()))
  }

  lazy val z3pyListOutput:PackratParser[List[Expression]] = "[[" ~ expressionList ~ "]]" ^^
  {
    case "[[" ~ exprLs ~ "]]" => exprLs
  }

  ///////////////////////////////
  // types
  lazy val typeClass:PackratParser[TypeClass] = integerType|booleanType|arrayType
  lazy val integerType = "Integer" ^^ {t => IntegerType()}
  lazy val booleanType = "Boolean" ^^ {t => BooleanType()}
  lazy val arrayType = "Array" ~> ("<" ~> typeClass <~ ">") ^^ {t => ArrayType(t)}

  ///////////////////////////////
  // function
  lazy val parameter:PackratParser[Parameter] = identifier ~ (":" ~> typeClass) ^^
  {
    case id ~ tpe => Parameter(id, tpe)
  }

  lazy val verificationStmt = assume|ensure
  lazy val verificationStmtList:Parser[List[VerificationStatment]] =
    "[" ~> repsep(verificationStmt, ",") <~ "]" ^^ {t => t}

  lazy val arguments:Parser[List[Parameter]] = repsep(parameter, ",")
  lazy val function:PackratParser[Function] =
    "function" ~ identifier ~ "(" ~ arguments ~ ")" ~ ":" ~ typeClass ~ (verificationStmtList ?) ~ sequence ^^
  {
    case f ~ name ~ op ~ args ~ cp ~ cl ~ tpe ~ vs ~ body =>
      vs match
      {
        case Some(l) => Function(name, args, tpe, body, l)
        case None    => Function(name, args, tpe, body)
      }
  }

  /////////////////////////////
  // statement
  lazy val statement:PackratParser[Statement] = invokeStm|assert|assume|ensure|returns|varDecl|whileLoop2|
                                                whileLoop|ifElse|assignment|sequence

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

  lazy val ensure:PackratParser[Ensure] = "ensure" ~> ("(" ~> expression <~ ")") ^^
  {
    case exp  => Ensure(exp)
  }

  lazy val returns:PackratParser[Return] = "return" ~> ("(" ~> expression <~ ")") ^^
  {
    case exp  => Return(exp)
  }

  lazy val varDecl:PackratParser[VariableDeclaration] = "local" ~> parameter ^^
  {
    case p => VariableDeclaration(p.name, p.typeClass)
  }

  lazy val invokeStm:PackratParser[InvokeStatement] = identifier ~ "." ~ identifier ~ ("(" ~> expressionList <~ ")")  ^^
  {
    case modName ~ _ ~ funcName ~ args => InvokeStatement(modName, funcName, args)
  }

  lazy val sequence:PackratParser[Sequence] = ("{" ~> (statement *) <~ "}") ^^
  {
    case s:Seq[Statement] => Sequence(s: _*)
  }

  lazy val whileLoop:PackratParser[While] = (("while(" ~> expression <~ ")") ~ statement) ^^
  {
    case exp ~ stm => While(null, null, exp, stm)
  }

  lazy val whileLoop2:PackratParser[While] =  "while(" ~ expression ~ "," ~loopPredicateAndInvariant ~")" ~ statement ^^
  {
    case "while(" ~ exp ~ "," ~ pi ~ ")" ~ stm => While(pi.predicates, pi.invariant, exp, stm)
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

  lazy val module:PackratParser[Module] = ("module(" ~> identifier <~ ")") ~ ("{" ~> (function *) <~ "}") ^^
  {
    case name ~ funcs => Module(name, funcs)
  }



  //////////////////////////////////////////////////
  // parsing call
  def parse(src:String):Module=
  {
    parseAll(module, src) match
    {
      case Success(topNode, _) => topNode
      case f                   => sys.error("error while parsing: " + f)
    }
  }

  def parseFunction(src:String):Function=
  {
    parseAll(function, src) match
    {
      case Success(topNode, _) => topNode
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

  def parsePredicates(src:String):Predicates =
  {
    parseAll(predicates, src) match
    {
      case Success(topNode, _) => topNode
      case f                   => sys.error("error while parseing: " + f)
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

  def parsePyZ3ListOutput(src:String):List[Expression]=
  {
    parseAll(z3pyListOutput, src) match
    {
      case Success(topNode, _) => topNode
      case f                   => println(src)
                                  sys.error("expression is not a z3pyListOutput expression")
    }
  }

  def parsePyZ3GenVar(src:String):Variable =
  {
    parseAll(z3pySysGenVar, src) match
    {
      case Success(topNode, _) => topNode
      case f                   => sys.error("expression is not a z3pySysGenVar expression")
    }
  }

}


