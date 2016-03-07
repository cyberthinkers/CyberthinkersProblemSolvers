package org.cyberthinkers.problemsolvers.cassowary

case class SymbolicWeight(val self: Double) extends AnyVal

object SymbolicWeight {
  def apply(w1: Int, w2: Int, w3: Int) = Strength(w1 << 20 | w2 << 10 | w3)
}
