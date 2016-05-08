package psksvp.Verica.QuantifierElimination

/**
  * Created by psksvp on 4/05/2016.
  */
import psksvp.Verica.Lang._
import psksvp.Verica._

case class Exists(variables: Variable*)
{
  override def toString:String=
  {
    var output = ""
    for(v <- variables)
      output = output.concat(s"(E $v)")
    output
  }
}

case class ForAll(variables: Variable*)
{
  override def toString:String=
  {
    var output = ""
    for(v <- variables)
      output = output.concat(s"(A $v)")
    output
  }
}

case class SuchThat(expression: Expression)
{
  override def toString:String = s"[$expression]"
}


object Solve
{
  def apply(exists: Exists, suchThat: SuchThat):String=
  {
    makeString(exists, suchThat)
  }

  private def makeString(exists: Exists, suchThat: SuchThat):String=
  {
    val output = "[AutoGenerateFromVerica]\n"
    val nonFreeVar = exists.variables.toSet
    val freeVar = listOfVariablesIn(suchThat.expression).toSet diff nonFreeVar
    output.concat(s"(${makeString(freeVar)},${makeString(nonFreeVar)})\n")
           .concat(s"${freeVar.size}\n")
           .concat(s"$exists$suchThat.\n")
           .concat(s"finish\n")
  }

  private def makeString(variables: Set[Variable]):String=
  {
    var output = ""
    for(v <- variables)
      output = output.concat(s"$v, ")

    val idx = output.lastIndexOf(',')
    output.substring(idx)
  }
}


