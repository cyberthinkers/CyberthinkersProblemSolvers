package org.cyberthinkers.problemsolvers.cassowary

import scala.collection.mutable

class SimplexSolver(
    val tableau: Tableau,
    val stayMinusErrorVars: mutable.ArrayBuffer[SlackVariable],
    val stayPlusErrorVars: mutable.Buffer[SlackVariable],
    val errorVars: mutable.HashMap[Constraint, mutable.HashSet[AbstractVariable]],
    val markerVars: mutable.HashMap[Constraint, AbstractVariable],
    val resolvePair: mutable.Buffer[Double],
    val editVarMap: mutable.HashMap[Variable, EditInfo],
    val variableFactory: VariableFactory,
    val objective: ObjectiveVariable,
    // FIXME: rework this stuff...
    var needsSolving: Boolean,
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
  protected def newExpression(cn: Constraint) = { // fixme: this function needs to be folded into Tableau
    val cnExpr = cn.expression
    var expr = new LinearExpression(cnExpr.constant) //<< fixme: rework to val instead of var
    cnExpr.terms.keys foreach { v =>
      val c = cnExpr.terms(v)
      val e = this.tableau.rows.get(v)
      if(!e.isDefined) {
        expr = expr.addVariable(v, c) 
      } else {
        expr = expr.addExpression(e.get, c)
      }
    }
    if(cn.isInstanceOf[LinearInequality]) {
      val slackVar = variableFactory.newSlackVariable()
      expr = expr.setVariable(slackVar, -1)
      markerVars.put(cn, slackVar)
      if(!cn.isRequired) {
        val eminus = variableFactory.newSlackVariable()
        expr = expr.setVariable(eminus, 1)
        val zRow = tableau.rows(objective)
        val sw = cn.strength.symbolicWeight * cn.weight
        zRow.setVariable(eminus, sw)
        insertErrorVar(cn, eminus)
        this.tableau.noteAddedVariable(eminus, objective);
      }
    }
  }
  
  protected def insertErrorVar(cn: Constraint, v: AbstractVariable) = {
    val cnset = errorVars.get(cn)
    val cnset2 = (if(cnset.isDefined) cnset.get else new mutable.HashSet[AbstractVariable]) + v
    errorVars.put(cn, cnset2)
  }
}