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
    var needsSolving: Boolean,
    var objective: ObjectiveVariable,
    var optimizeAutomatically: Boolean) {
  

  
}