package org.cyberthinkers.problemsolvers.cassowary

abstract class Constraint {
  val strength: Strength
  val weight: Double
  val expression: LinearExpression
  def isRequired = strength == Strength.required
}

abstract class EditOrStayConstraint(
  variable: AbstractVariable, strength: Strength, weight: Double, val expression: LinearExpression) extends Constraint {
}

case class EditConstraint(
  val variable: Variable, val strength: Strength = Strength.required, val weight: Double = 1)
  extends EditOrStayConstraint(variable, strength, weight, new LinearExpression(variable, -1, variable.value)) {
}

case class StayConstraint(
  val variable: Variable, val strength: Strength = Strength.required, val weight: Double = 1) 
  extends EditOrStayConstraint(variable, strength, weight, new LinearExpression(variable, -1, variable.value)) {
}

case class LinearConstraint(
  val expression: LinearExpression, val strength: Strength = Strength.required, val weight: Double = 1) extends Constraint {
}

case class LinearInequality(
  val expression: LinearExpression, val strength: Strength = Strength.required, val weight: Double = 1) extends Constraint {
}

object LinearInequality {
  def <=(clv: AbstractVariable, cle: LinearExpression): LinearInequality = {
    LinearInequality((cle * -1) + clv)
  }
  def >=(clv: AbstractVariable, cle: LinearExpression): LinearInequality = {
    LinearInequality(cle + clv)
  }
}
