package org.cyberthinkers.problemsolvers.cassowary

abstract class AbstractVariable extends Ordered[AbstractVariable] with Equals {
  val id: Int
  
  final def isDummy = this match { case _: DummyVariable => true; case _ => false }
  final def isExternal = this match { case _: Variable => true; case _ => false }
  final def isPivotable = this match { case _: SlackVariable => true; case _ => false }
  final def isRestricted = this match { case _: SlackVariable => true; case _: DummyVariable => true; case _ => false }
  
  override def compare(v: AbstractVariable) = id.compareTo(v.id)
  override def canEqual(v: Any) = v.isInstanceOf[AbstractVariable]
  override def equals(v: Any) = v match {
    case that: AbstractVariable => this.id == that.id
    case _                      => false
  }
  override def hashCode = id.hashCode()
}

object AbstractMutableVariable {
  val epsilon = 1e-8

  //FIXME - this needs to be removed from here, and perhaps have an implicit context injected into this somehow from simplexSolver
  private var nextValue: Int = 0;
  private def getNextValue(): Int = { synchronized { nextValue += 1; nextValue } }
}

abstract class AbstractMutableVariable extends AbstractVariable {
  import AbstractMutableVariable.epsilon
  var value: Double // add value
    
  def approxEqual(thatValue: AbstractMutableVariable) = Math.abs(this.value - thatValue.value) < epsilon
  def isApproxZero: Boolean = Math.abs(this.value) < epsilon
 }

case class DummyVariable(var value: Double, val id: Int) extends AbstractMutableVariable

case class ObjectiveVariable(var value: Double, val id: Int) extends AbstractMutableVariable

case class SlackVariable(var value: Double, val id: Int) extends AbstractMutableVariable

case class Variable(var value: Double, val id: Int) extends AbstractMutableVariable

