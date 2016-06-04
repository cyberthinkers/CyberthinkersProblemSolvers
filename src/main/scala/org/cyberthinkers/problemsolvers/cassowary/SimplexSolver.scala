package org.cyberthinkers.problemsolvers.cassowary

import scala.collection.mutable

class SimplexSolver(
    val tableau: Tableau,
    val errorVars: mutable.HashMap[Constraint, mutable.HashSet[AbstractVariable]],
    val markerVars: mutable.HashMap[Constraint, AbstractVariable],
    val resolvePair: mutable.Buffer[Double],
    val editVarMap: mutable.HashMap[Variable, EditInfo],
    val objective: ObjectiveVariable,
    val variableFactory: VariableFactory,
    
    // FIXME: refactor this stuff...
    var needsSolving: Boolean,
    var optimizeAutomatically: Boolean) {

  /** Convenience function for creating linear inequality constraint with a lower bound */
  final def addLoweverBound(v: AbstractVariable, lower: Double): Unit = {
    tableau.addConstraint(LinearInequality.>=(v, new LinearExpression(lower)))
  }

  /** Convenience function for creating linear inequality constraint with an upper bound */
  final def addUpperBound(v: AbstractVariable, upper: Double): Unit = {
    tableau.addConstraint(LinearInequality.<=(v, new LinearExpression(upper)))
  }

  /** Convenience function for creating a linear inequality constraint with upper and lower bounds */
  final def addBounds(v: AbstractVariable, lower: Double, upper: Double): Unit = {
    addLoweverBound(v, lower)
    addUpperBound(v, upper)
  }

  final def addEditConstraint(cn: EditConstraint) = {
    tableau.addEditConstraint(cn)
  }

  final def addEditVar(v: Variable, strength: Strength = Strength.strong): Unit = {
    addEditConstraint(EditConstraint(v, strength))
  }
  
  

    /** Add constraint to solver */
//  protected def addConstraint(cn: Constraint): Unit = {
//    tableau.addConstraint(cn)
//    needsSolving = true
//    if(optimizeAutomatically) {
//      tableau.optimize(objective)
//      tableau.setExternalVariables();
//    }
//  }
  

 }