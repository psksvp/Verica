package psksvp.Verica.QuantifierElimination

/**
  * Created by psksvp on 4/05/2016.
  */
import psksvp.Verica.Lang._
import psksvp.Verica._

abstract class Quantifier(vars:Seq[Variable])
{
  def isTheir(variable: Variable) = if(vars.indexOf(variable) >= 0) true else false
  override def toString:String=
  {
    if(1 == vars.size)
      vars(0).name
    else
    {
      var output = "["
      for (v <- vars)
        output = output.concat(v + " ")
      output.concat("]")
    }
  }
}

case class Exists(variables: Seq[Variable]) extends Quantifier(variables)
case class ForAll(variables: Seq[Variable]) extends Quantifier(variables)
case class SuchThat(expression: Expression)
{
  override def toString:String = s"[$expression]"
}

object QE
{
  def apply(quantifier: Quantifier, suchThat: SuchThat):String=
  {
    import sys.process._
    makeZ3Python(quantifier, suchThat).!!
  }

  private def makeZ3Python(quantifier: Quantifier, suchThat: SuchThat):String=
  {
    def makeVariables:String=
    {
      var sVar = quantifier.toString + " "
      for(v <- listOfVariablesIn(suchThat.expression))
      {
        if(false == quantifier.isTheir(v))
          sVar = sVar.concat(v + " ")
      }
      sVar.trim
    }


    val output = "/usr/bin/python -c \"from z3 import *;" +
                   makeVariables.replaceAll(" ", ",") + " = Reals('" + makeVariables + "');" +
                  "g = Goal();" +
                  "g.add(%s);" +
                  "t = Tactic('qe');" +
                  "print(t(g));\""

    val qfType = quantifier match
    {
      case e:Exists => "Exists(" + e.toString + "," + z3Pythonize(suchThat.expression) + ")"
      case e:ForAll => "ForAll(" + e.toString + "," + z3Pythonize(suchThat.expression) + ")"
    }

    String.format(output, qfType).trim()
  }
}


/*
case class Exists(variables: Seq[Variable])
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


object QE
{
  def apply(exists: Exists, suchThat: SuchThat):Expression=
  {
    import sys.process._
    val cmd = makeString(exists, suchThat)
    "qepcad" #< cmd  !!
  }

  private def makeString(exists: Exists, suchThat: SuchThat):String=
  {
    val output = "[AutoGenerateFromVerica]\n"
    val nonFreeVar = exists.variables.toSet
    val freeVar = listOfVariablesIn(suchThat.expression).toSet diff nonFreeVar
    output.concat(s"(${makeString(freeVar)}, ${makeString(nonFreeVar)})\n")
          .concat(s"${freeVar.size}\n")
          .concat(s"$exists$suchThat.\n")
          .concat(s"finish\n")
  }

  private def makeString(variables: Set[Variable]):String=
  {
    var output = ""
    for(v <- variables)
    {
      output = output.concat(v.toString + ",")
    }

    //output
    val idx = output.lastIndexOf(',')
    output.substring(0, idx)
  }
} */


