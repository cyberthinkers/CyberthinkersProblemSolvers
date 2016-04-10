package org.cyberthinkers.problemsolvers.cassowary

import scala.collection.mutable

class SimplexSolver(
    val tableau: Tableau,
    val stayMinusErrorVars: mutable.ArrayBuffer[SlackVariable],
    val stayPlusErrorVars: mutable.Buffer[SlackVariable],
    val errorVars: mutable.HashMap[Constraint, mutable.Set[Any]],
    val markerVars: mutable.HashMap[Constraint, mutable.Set[Variable]],
    val resolvePair: mutable.Buffer[Double],
    val editVarMap: mutable.HashMap[Variable, EditInfo],
    // FIXME: rework this stuff...
    var needsSolving: Boolean,
    var objective: ObjectiveVariable,
    var optimizeAutomatically: Boolean) {
  
  /**
   * Add constraint to solver
   */
   def addConstraint(cn: Constraint) = {
    
  }
   
 /*
  * Convenience function for creating a lower bound, linear inequality constraint
  */
  def addLoweverBound(v: AbstractVariable, lower: Double) = {
    addConstraint(LinearInequality.>=(v, new LinearExpression(lower)))
  }
  
 /*
  * Convenience function for creating an upper bound, linear inequality constraint
  */
  def addUpperBound(v: AbstractVariable, upper: Double) = {
    addConstraint(LinearInequality.<=(v, new LinearExpression(upper)))
  }
  
 /*
  * Convenience function for creating an lower/upper bound, linear inequality constraint
  */
  def addBounds(v: AbstractVariable, lower: Double, upper: Double) = {
    addLoweverBound(v, lower)
    addUpperBound(v, upper)
  }
  
  protected def newExpression(cn: Constraint, prevEConstant: Double) = {
    val cnExpr = cn.expression
    val expr = new LinearExpression(cnExpr.constant)
    cnExpr.terms.keys foreach { v =>
      val c = cnExpr.terms(v)
//      val e = rowExpression(v)
    }
  }
  
}