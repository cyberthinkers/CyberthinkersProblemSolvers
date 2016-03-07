package org.cyberthinkers.problemsolvers.cassowary

import scala.collection.immutable.HashMap

// add implicit conversion somewhere, from Variable to LinearExpression

object LinearExpression {
  @inline def approxEqual(a: Double, b: Double): Boolean = Math.abs(a - b) < AbstractMutableVariable.epsilon
  @inline def isApproxZero(a: Double) = Math.abs(a) < AbstractMutableVariable.epsilon
}

case class LinearExpression(val terms: Map[AbstractVariable, Double], val constant: Double) {
  import  org.cyberthinkers.problemsolvers.cassowary.LinearExpression._

  def this(constant: Double) = this(new HashMap[AbstractVariable, Double], constant)

  def this() = this(0);

  def this(v: AbstractVariable, value: Double = 1) = this(HashMap(v -> value), 0)

  def isConstant: Boolean = terms.size == 0

  def *(value: Double): LinearExpression = LinearExpression(for ((k, v) <- this.terms) yield (k, v * value), constant * value)

  def *(expression: LinearExpression): LinearExpression = {
    if (expression isConstant) this * expression.constant
    else require(!(this isConstant), "Can't allow nonlinear expressions"); expression * this.constant
  }

  def +(variable: Variable) = addExpression(new LinearExpression(variable))

  def -(variable: Variable) = addExpression(new LinearExpression(variable), -1)
  
  def /(d: Double) = this * (1.0 / d) //FIXME removed / 0 test, but probably should be restated as a partial function
  
  // missing / LinearExpression - Is it really needed here?
  
  def +(expression: LinearExpression) = addExpression(expression)

  def -(expression: LinearExpression) = addExpression(expression, -1)

  //FIXME- Missing addExpression with Tableau solver - rework addExpression or Tableau
  
  def addExpression(expression: LinearExpression, n: Double = 1) = {
    val v1 =
      for {
        (v, coeff) <- expression.terms
        c = coeff * n
        cld = terms.get(v)
        if (cld.isDefined && !isApproxZero(c)) // guard - don't add any new items if c is approximately zero
        revisedCoeff = if (cld.isDefined) cld.get + c else c
      } yield (v, revisedCoeff)
    val v2 = terms ++ v1
    val v3 = v2.filter(v => !isApproxZero(v._2))
    LinearExpression(v3, n + constant)
  }

  def anyPivotableVariable = terms.find(p => p._1 isPivotable)
  
  // missing substituteOut
  
  def changeSubject(oldSubject: AbstractVariable,  newSubject: AbstractVariable) = {
    val (revisedLinearExpression, reciprocal) = setNewSubject(newSubject)
    LinearExpression(revisedLinearExpression.terms + (oldSubject -> reciprocal), revisedLinearExpression.constant)
  }
    
  def setNewSubject(subject: AbstractVariable) = {
    val coeff = terms.get(subject).get
    val revisedExpr = LinearExpression(terms - subject, constant)
    val reciprocal = 1.0 / coeff
    val revisedLinearExpression = revisedExpr * -reciprocal
    (revisedLinearExpression, reciprocal)
  }
}
