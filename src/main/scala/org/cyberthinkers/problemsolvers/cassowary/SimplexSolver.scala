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
    val (expr, eplus_eminus, prevEConstant) = newExpression(cn)
    
  }
  
  protected def add(expr: LinearExpression) = {
    val (subject, exprRevised) = chooseSubject(expr)
    if(subject.isDefined) { // if(we added directly)
      
    } else { // else, try indirectly
      
    }
  }
  
  protected def chooseSubject(expr: LinearExpression): (Option[AbstractVariable], LinearExpression) = {
    // FIXME: this function should be split apart and expr handled differently
    var foundUnrestricted = false
    var foundNewRestricted = false
    var subject: Option[AbstractVariable] = Option.empty
    var continueSearch = true
    for (v <- expr.terms.keys; c = expr.terms(v) if (continueSearch)) {
      if (foundUnrestricted) {
        if (!v.isRestricted) {
          if (!this.tableau.columns.contains(v)) {
            subject = Some(v)
            continueSearch = false
          }
        }
      } else { // we haven't found a restricted variable yet
        if (v.isRestricted) {
          if (!foundNewRestricted && !v.isDummy && c < 0.0) {
            val col = this.tableau.columns.get(v)
            if (!col.isDefined || (col.get.size == 1 && this.tableau.columns.contains(this.objective))) {
              subject = Some(v)
              foundNewRestricted = true
            }
          }
        } else {
          subject = Some(v)
          foundUnrestricted = true
        }
      }
    }
    if (!subject.isDefined) {
      var coeff = 0.0
      val noneDummy = expr.terms.keys.find { v => !v.isDummy }
      if (noneDummy.isEmpty) {
        for (v <- expr.terms.keys; c = expr.terms(v)) {
          if (!this.tableau.columns.contains(v)) {
            subject = Some(v)
            coeff = c
          }
        }
      }
      require(!LinearExpression.isApproxZero(expr.constant), "ExCLRequiredFailure")
      (subject, if (coeff > 0.0) expr * -1 else expr)
    } else {
      (subject, expr)
    }
  }

  protected def newExpression(cn: Constraint) = { // fixme: this function needs to be folded into Tableau
    val eplus_eminus: mutable.Buffer[SlackVariable] = mutable.Buffer.empty
    val cnExpr = cn.expression
    var expr = new LinearExpression(cnExpr.constant) //<< fixme: rework to val instead of var
    var prevEConstant = 0.0;
    cnExpr.terms.keys foreach { v =>
      val c = cnExpr.terms(v)
      val e = this.tableau.rows.get(v)
      if (!e.isDefined) {
        expr = expr.addVariable(v, c)
      } else {
        expr = expr.addExpression(e.get, c)
      }
    }
    if (cn.isInstanceOf[LinearInequality]) {
      val slackVar = variableFactory.newSlackVariable()
      expr = expr.setVariable(slackVar, -1)
      markerVars.put(cn, slackVar)
      if (!cn.isRequired) {
        val eminus = variableFactory.newSlackVariable()
        expr = expr.setVariable(eminus, 1.0)
        val zRow = this.tableau.rows(objective)
        val sw = cn.strength.symbolicWeight * cn.weight
        this.tableau.rows += objective -> zRow.setVariable(eminus, sw)
        insertErrorVar(cn, eminus)
        this.tableau.noteAddedVariable(eminus, objective);
      }
    } else {
      if (cn.isRequired) {
        val dummyVar = variableFactory.newDummyVariable()
        expr = expr.setVariable(dummyVar, 1.0)
        markerVars.put(cn, dummyVar)
      } else {
        val eplus = variableFactory.newSlackVariable()
        val eminus = variableFactory.newSlackVariable()
        expr = expr.setVariable(eplus, -1.0)
        expr = expr.setVariable(eminus, 1.0)
        markerVars.put(cn, eplus)
        val zRow = tableau.rows(objective)
        val sw = cn.strength.symbolicWeight * cn.weight
        this.tableau.rows += objective -> zRow.setVariable(eplus, sw)
        this.tableau.noteAddedVariable(eplus, objective);
        this.tableau.rows += objective -> zRow.setVariable(eminus, sw)
        this.tableau.noteAddedVariable(eminus, objective);
        insertErrorVar(cn, eminus)
        insertErrorVar(cn, eplus)
        if (cn.isInstanceOf[StayConstraint]) {
          stayPlusErrorVars += eplus
          stayMinusErrorVars += eminus
        } else if (cn.isInstanceOf[EditConstraint]) {
          eplus_eminus += eplus
          eplus_eminus += eminus
          prevEConstant = cnExpr.constant
        }
      }
    }
    expr = if (expr.constant < 0) expr * -1 else expr
    (expr, eplus_eminus, prevEConstant)
  }

  protected def insertErrorVar(cn: Constraint, v: AbstractVariable) = {
    val cnset = errorVars.get(cn)
    val cnset2 = (if (cnset.isDefined) cnset.get else new mutable.HashSet[AbstractVariable]) + v
    errorVars.put(cn, cnset2)
  }
}