package psksvp.Verica.Z3

/**
  * Created by psksvp on 4/05/2016.
  */
import psksvp.Verica.Lang._
import psksvp.Verica._

abstract class Quantifier(vars:Seq[Variable])
{
  def isTheir(variable: Variable) = if(vars.indexOf(variable) >= 0) true else false
  def allVariables = vars
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
  def solve(quantifier: Quantifier, suchThat: SuchThat):Expression=
  {
    // do not simplify the following calls
    // they are meant for easy debugging
    val code = makeZ3Python(quantifier, suchThat)
    val result = psksvp.evalPython(code)
    val pyExpr = Parser.parseZ3ListOutput(result)
    and(pyExpr)
  }

  private def makeZ3Python(quantifier: Quantifier, suchThat: SuchThat):String=
  {
    // here I assume that every variable is an Interger (Ints)
    // z3py has Real, Reals, Int, Ints, Bool, Bools
    val varDecl = Z3.makeIntVariables(suchThat.expression) + ";" + Z3.makeIntVariables(quantifier.allVariables)
    val qfType = quantifier match
    {
      case e:Exists => "Exists(" + e.toString + "," + Z3.pythonize(suchThat.expression) + ")"
      case e:ForAll => "ForAll(" + e.toString + "," + Z3.pythonize(suchThat.expression) + ")"
    }
    s"""
      |from z3 import *
      |$varDecl
      |gOaL3fgrt = Goal()
      |gOaL3fgrt.add($qfType)
      |t = Tactic('qe')
      |print(t(gOaL3fgrt))
    """.stripMargin.trim
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


