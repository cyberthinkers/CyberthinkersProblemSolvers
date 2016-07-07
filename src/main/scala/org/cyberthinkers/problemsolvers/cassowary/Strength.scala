package org.cyberthinkers.problemsolvers.cassowary

final case class Strength(symbolicWeight: Double) extends AnyVal {
  
}

object Strength {
  def apply(w1: Int, w2: Int, w3: Int) = new Strength(w1 << 20 | w2 << 10 | w3)
  val required = Strength(1000, 1000, 1000)
  val strong = Strength(1, 0, 0)
  val medium = Strength(0, 1, 0)
  val weak = Strength(0, 0, 1)
}

