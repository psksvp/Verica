package psksvp.Verica.Lang


/**
  * Created by psksvp on 11/04/2016.
  */
abstract class Node(_children:List[Node])
{
  private val uuid = "L" + java.util.UUID.randomUUID().toString().replace("-", "")
  def id = uuid
  def children = _children
  override def toString=Prettified(this)
}


abstract class Operator(val symbol:String) extends Node(Nil)

case class Plus() extends Operator("+")
case class Minus() extends Operator("-")
case class Multiply() extends Operator("*")
case class Division() extends Operator("/")

//relational
case class Equal() extends Operator("==")
case class Greater() extends Operator(">")
case class Less() extends Operator("<")
case class GreaterOrEqual() extends Operator(">=")
case class LessOrEqual() extends Operator("<=")
case class NotEqual() extends Operator("!=")

//logical
case class Negation() extends Operator("¬")
case class Or() extends Operator("⋁")
case class And() extends Operator("⋀")

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

case class Literal(representation:String) extends Expression(Nil)
case class Variable(name:String) extends Expression(Nil)

case class Unary(operator: Operator,
                 expr:Expression) extends Expression(List(operator, expr))

case class Binary(operator: Operator,
                  exprLeft:Expression,
                  exprRight:Expression) extends Expression(List(operator, exprLeft, exprRight))


case class Invariant(expr:Expression) extends Expression(List(expr))
case class Predicates(exprs:Expression*) extends Expression(exprs.toList)
{
  def count = exprs.size
  def apply(index:Int) = exprs(index)
}


abstract class Statement(children:List[Node]) extends Node(children)
case class Empty() extends Statement(Nil)
case class Assignment(variable:Variable, expr:Expression) extends Statement(List(variable, expr))
case class Assert(expr:Expression) extends Statement(List(expr))
case class Assume(expr:Expression) extends Statement(List(expr))
case class Sequence(stmts:Statement*) extends Statement(stmts.toList)
case class Choice(stmtA:Statement, stmtB:Statement) extends Statement(List(stmtA, stmtB))

case class While(predicates:Predicates,
                 invariant:Invariant,
                 expr:Expression,
                 stmt:Statement) extends Statement(List(expr, stmt))

case class Module(name:String,
                  sequence:Sequence) extends Node(List(sequence))


abstract class Formula(children:List[Node]) extends Expression(children)
