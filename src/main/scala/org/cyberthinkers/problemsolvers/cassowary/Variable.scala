package org.cyberthinkers.problemsolvers.cassowary

object AbstractVariable {
   final val epsilon = 1e-8
}

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

final case class DummyVariable(val id: Int) extends AbstractVariable

final case class ObjectiveVariable(val id: Int) extends AbstractVariable

final case class SlackVariable(val id: Int) extends AbstractVariable

final case class Variable(val value: Double, val id: Int) extends AbstractVariable {
  final def approxEqual(thatValue: Variable) = Math.abs(this.value - thatValue.value) < AbstractVariable.epsilon
  final def isApproxZero: Boolean = Math.abs(this.value) < AbstractVariable.epsilon
}

