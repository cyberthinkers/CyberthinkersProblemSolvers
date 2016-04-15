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
    val VariableFactory: VariableFactory,
    // FIXME: rework this stuff...
    var needsSolving: Boolean,
    var objective: ObjectiveVariable,
    var optimizeAutomatically: Boolean) {
  
  
  
  /** Convenience function for creating linear inequality constraint with a lower bound */
  def addLoweverBound(v: AbstractVariable, lower: Double) = {
    addConstraint(LinearInequality.>=(v, new LinearExpression(lower)))
  }
  
  /** Convenience function for creating linear inequality constraint with an upper bound */
  def addUpperBound(v: AbstractVariable, upper: Double) = {
    addConstraint(LinearInequality.<=(v, new LinearExpression(upper)))
  }
  
  /** Convenience function for creating a linear inequality constraint with upper and lower bounds */
  def addBounds(v: AbstractVariable, lower: Double, upper: Double) = {
    addLoweverBound(v, lower)
    addUpperBound(v, upper)
  }
  
   /** Add constraint to solver */
   def addConstraint(cn: Constraint) = {
    
  }
   // returns :(Vector[SlackVariable], Double)
  protected def newExpression(cn: Constraint) = {
    val cnExpr = cn.expression
    val expr = new LinearExpression(cnExpr.constant)
    cnExpr.terms.keys foreach { v =>
      val c = cnExpr.terms(v)
      val e = this.tableau.rows.get(v)
      val revisedExpr = if(!e.isDefined) {
        expr.addVariable(v, c) 
      } else {
        expr.addExpression(e.get, c)
      }
    }
    if(cn.isInstanceOf[LinearInequality]) {
      val slackVar = VariableFactory.newSlackVariable()
      expr.addVariable(slackVar, -1)
      
    }
  }
  
}