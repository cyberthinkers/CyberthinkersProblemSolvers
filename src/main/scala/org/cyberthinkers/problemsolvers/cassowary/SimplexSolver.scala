package org.cyberthinkers.problemsolvers.cassowary

import scala.collection.mutable.{HashMap => MutableHashMap}

abstract class SimplexSolver {
    var tableau: Tableau
    var stayMinusErrorVars: Vector[SlackVariable]
    var stayPlusErrorVars: Vector[SlackVariable]
    var errorVars: MutableHashMap[Constraint, Set[Any]]
    var markerVars: MutableHashMap[Constraint, Set[Variable]]
    var resolvePair: Vector[Double]
    var objective: ObjectiveVariable
    var editVarMap: MutableHashMap[Variable, EditInfo]
    var needsSolving: Boolean
    var optimizeAutomatically: Boolean

  
}