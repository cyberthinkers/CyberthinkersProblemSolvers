package org.cyberthinkers.problemsolvers.cassowary

import scala.collection.immutable.HashMap

// add implicit conversion somewhere, from Variable to LinearExpression

object LinearExpression {
  @inline def approxEqual(a: Double, b: Double) = Math.abs(a - b) < AbstractVariable.epsilon
  @inline def isApproxZero(a: Double) = Math.abs(a) < AbstractVariable.epsilon
}

final case class LinearExpression(val terms: Map[AbstractVariable, Double], val constant: Double) {
  import  org.cyberthinkers.problemsolvers.cassowary.LinearExpression._

  def this(constant: Double) = this(new HashMap[AbstractVariable, Double], constant)

  def this() = this(0);

  def this(v: AbstractVariable, value: Double = 1) = this(Map(v -> value), 0)
  
  def this(v: AbstractVariable, value: Double, constant: Double) = this(Map(v -> value), constant)

  def isConstant: Boolean = terms.size == 0

  def *(value: Double): LinearExpression = LinearExpression(for ((k, v) <- this.terms) yield (k, v * value), constant * value)

  def *(expression: LinearExpression): LinearExpression = {
    if (expression isConstant) this * expression.constant
    else require(!(this isConstant), "Can't allow nonlinear expressions"); expression * this.constant
  }

  def +(variable: AbstractVariable): LinearExpression = addExpression(new LinearExpression(variable))

  def -(variable: AbstractVariable): LinearExpression = addExpression(new LinearExpression(variable), -1)
  
  def /(d: Double): LinearExpression = this * (1.0 / d) //FIXME removed / 0 test
  
  // missing / LinearExpression - Is it really needed here?
  
  def +(expression: LinearExpression) = addExpression(expression)

  def -(expression: LinearExpression) = addExpression(expression, -1)
  
  def --(variable: AbstractVariable): LinearExpression = {
    val newTerms = terms - variable;
    LinearExpression(newTerms, constant);
  }

  def addVariable(v: AbstractVariable, c: Double):LinearExpression = {
    val coeff = terms.get(v)
      if(coeff.isDefined) {
        val newCoefficient = coeff.get + c
        if(isApproxZero(newCoefficient)) {
          LinearExpression(terms - v, this.constant)
        } else {
          LinearExpression(terms + (v -> newCoefficient), this.constant)
        }
      } else {
          if(!isApproxZero(c)) {
             LinearExpression(terms + (v -> c), this.constant)
          } else {
            this
          }
      } 
  }
  
  def setVariable(v: AbstractVariable, c: Double):LinearExpression = LinearExpression(terms + (v -> c), this.constant)
  
  def addExpression(expression: LinearExpression, n: Double = 1): LinearExpression = {
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
  
  def changeSubject(oldSubject: AbstractVariable,  newSubject: AbstractVariable): LinearExpression = {
    val coeff = terms(newSubject)
    val revisedExpr = LinearExpression(terms - newSubject, constant)
    val reciprocal = 1.0 / coeff
    val revisedLinearExpression = revisedExpr * -reciprocal
    LinearExpression(revisedLinearExpression.terms + (oldSubject -> reciprocal), revisedLinearExpression.constant)
  }
  
  def newSubject(subject: AbstractVariable): (LinearExpression, Double) = {
    val coeff = terms(subject)
    val reciprocal = 1.0 / coeff
    (LinearExpression(terms - subject, constant) * -reciprocal, reciprocal)
  }
  
  def coefficientFor(v: AbstractVariable) = terms.getOrElse(v, 0.0)
}
