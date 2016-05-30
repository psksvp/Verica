package psksvp.Verica.Lang


/**
  * Created by psksvp on 11/04/2016.
  */
abstract class Node(_children:List[Node])
{
  private val uuid = "L" + java.util.UUID.randomUUID().toString().replace("-", "")
  def id = uuid
  def children = _children
  override def toString=Prettified(this).replaceAll("(?m)^\\s*$[\n\r]{1,}", "")
}


abstract class Operator(val symbol:String) extends Node(Nil)

case class Plus() extends Operator("+")
case class Minus() extends Operator("-")
case class Multiply() extends Operator("*")
case class Division() extends Operator("/")
case class Power()    extends Operator("^")

//relational
case class Equal() extends Operator("=")
case class Greater() extends Operator(">")
case class Less() extends Operator("<")
case class GreaterOrEqual() extends Operator(">=")
case class LessOrEqual() extends Operator("<=")
case class NotEqual() extends Operator("!=")

//logical
case class Negation() extends Operator("~")
case class Or() extends Operator("""\/""")
case class And() extends Operator("""/\""")
case class Implies() extends Operator("->")

abstract class Expression(children:List[Node]) extends Node(children)

import scala.reflect.runtime.universe._
abstract class Value[T:TypeTag](_value:T) extends Expression(Nil)
{
  def selfType:Type = typeOf[T]
  def value:T = _value
}

case class IntegerValue(v:Int) extends Value[Int](v)
abstract class BooleanValue(v:Boolean) extends Value[Boolean](v)

case class True() extends BooleanValue(true)
case class False() extends BooleanValue(false)

abstract class TypeClass(name:String) extends Node(Nil)
{
  override def toString=name
}
case class IntegerType() extends TypeClass("Integer")
case class BooleanType() extends TypeClass("Boolean")
case class ArrayType(typeClass:TypeClass) extends TypeClass(s"Array<$typeClass>")

case class Literal(representation:String) extends Expression(Nil)

case class Unary(operator: Operator,
                 expr:Expression) extends Expression(List(operator, expr))

case class Binary(operator: Operator,
                  exprLeft:Expression,
                  exprRight:Expression) extends Expression(List(operator, exprLeft, exprRight))

abstract class VariableKind extends Node(Nil)
case class ArrayVariable() extends VariableKind
case class ValueVariable() extends VariableKind

case class Variable(name:String,
                    index:List[Expression] = Nil,
                    kind:VariableKind = ValueVariable()) extends Expression(Nil)

case class Length(v:Variable) extends Expression(List(v))

case class InvokeExpression(moduleName:String,
                          functionName:String,
                            parameters:List[Expression]) extends Expression(parameters)


case class Parameter(name:String, typeClass:TypeClass) extends Node(Nil)
case class Function(name:String,
                    parameters:List[Parameter],
                    typeClass:TypeClass,
                    body:Statement) extends Node(Nil)
////////////////////////////////////////////////////////////////////////
case class Predicates(exprs:Expression*) extends Node(exprs.toList)
{
  def count = exprs.size
  def apply(index:Int) = exprs(index)
}

case class PredicatesAndInvariant(predicates: Predicates,
                                   invariant: Expression) extends Node(List(predicates, invariant))

abstract class Statement(children:List[Node]) extends Node(children)
case class VariableDeclaration(name:String, typeClass:TypeClass) extends Statement(Nil)
case class Empty() extends Statement(Nil)
case class Assignment(variable:Variable, expr:Expression) extends Statement(List(variable, expr))
case class Return(expression:Expression) extends Statement(List(expression))

abstract class VerificationStatment(val name:String,
                                    val expression:Expression) extends Statement(List(expression))
case class Assert(expr:Expression) extends VerificationStatment("assert", expr)
case class Assume(expr:Expression) extends VerificationStatment("assume", expr)
case class Ensure(expr:Expression) extends VerificationStatment("ensure", expr)

case class InvokeStatement(moduleName:String,
                           functionName:String,
                           parameters:List[Expression]) extends Statement(parameters)

case class Sequence(stmts:Statement*) extends Statement(stmts.toList)
{
  def count=stmts.size
}
case class Choice(stmtA:Statement, stmtB:Statement) extends Statement(List(stmtA, stmtB))

case class While(predicates:Predicates,
                 invariant:Expression,
                 expr:Expression,
                 stmt:Statement) extends Statement(List(expr, stmt))

case class If(e:Expression,
              stmtA:Statement,
              stmtB:Statement=Empty()) extends Statement(List(e, stmtA, stmtB))
{
  def toChoice:Choice=
  {
    import psksvp.Verica._
    Choice(Sequence(Assume(e), stmtA), Sequence(Assume(not(e)), stmtB))
  }
}

case class Module(name:String,
                  functions:List[Function]) extends Node(functions)
{
  def function(name:String):Option[Function] =
  {
    for(f <- functions)
    {
      if(f.name == name)
        return Some(f)
    }
    None
  }
}


