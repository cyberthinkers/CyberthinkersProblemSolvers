package org.cyberthinkers.problemsolvers.cassowary

abstract class Constraint {
  val strength: Strength
  val weight: Double
  def isRequired = strength == Strength.required
}

case class EditConstraint(
  val variable: AbstractMutableVariable, val strength: Strength = Strength.required, val weight: Double = 1) extends Constraint

case class StayConstraint(
  val variable: AbstractMutableVariable, val strength: Strength = Strength.required, val weight: Double = 1) extends Constraint

case class LinearConstraint(
  val linearExpression: LinearExpression, val strength: Strength = Strength.required, val weight: Double = 1) extends Constraint
