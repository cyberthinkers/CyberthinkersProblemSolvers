package org.cyberthinkers.problemsolvers.cassowary

import scala.collection.mutable

class Tableau(
    val variableFactory: VariableFactory,
    val objective: ObjectiveVariable,
    private[this] val columns: mutable.Map[AbstractVariable, mutable.Set[AbstractVariable]] = mutable.Map.empty,
    private[this] val rows: mutable.Map[AbstractVariable, LinearExpression] = mutable.Map.empty,
    private[this] val infeasibleRows: mutable.Set[AbstractVariable] = mutable.Set.empty,
    private[this] val editVarMap: mutable.Map[Variable, EditInfo] = mutable.Map.empty,
    private[this] val externalRows: mutable.Set[Variable] = mutable.Set.empty,
    private[this] val externalParametricVars: mutable.Set[Variable] = mutable.Set.empty,
    private[this] val errorVars: mutable.Map[Constraint, mutable.Set[AbstractVariable]] = mutable.Map.empty,
    private[this] val markerVars: mutable.Map[Constraint, AbstractVariable] = mutable.Map.empty,
    private[this] val stayMinusErrorVars: mutable.Buffer[SlackVariable] = mutable.ArrayBuffer.empty,
    private[this] val stayPlusErrorVars: mutable.Buffer[SlackVariable] = mutable.ArrayBuffer.empty) {

  final def addEditConstraint(cn: EditConstraint) = {
    val (eplus_eminus, prevEConstant) = addConstraint(cn)
    val size = editVarMap.size
    val cnEdit: EditConstraint = cn.asInstanceOf[EditConstraint]
    val clvEplus = eplus_eminus(0)
    val clvEminus = eplus_eminus(1)
    val e = EditInfo(cnEdit, clvEplus, clvEminus, prevEConstant, size)
    this.editVarMap += cnEdit.variable -> e
  }

  final def addEditVar(v: Variable, strength: Strength = Strength.strong): Unit = {
    addEditConstraint(EditConstraint(v, strength))
  }

  final def removeEditVar(v: Variable): Unit = {
    val cei = editVarMap(v)
    val cn = cei.constraint
    removeConstraint(cn)
  }

  /** Add constraint to solver */
  final def addConstraint(cn: Constraint) = {
    val (expr1, eplus_eminus, prevEConstant) = newExpression(cn, objective)
    // try adding directly without creating an artificial variable
    val (subject, expr2) = chooseSubject(expr1)
    if (subject.isDefined) { // if (we can add directly)
      substitueOut(subject.get, expr2)
      rows += subject.get -> expr2
    } else { // try adding indirectly with an artificial variable
      val av = variableFactory.newSlackVariable()
      val az = variableFactory.newObjectiveVariable()
      rows += av -> expr1
      rows += az -> expr1
    }
    (eplus_eminus, prevEConstant)
  }

  final def removeConstraint(cn: Constraint) = {

  }

  // FIXME: check should be made that the subject is in the tableau
  protected def noteRemovedVariable(v: AbstractVariable, subject: AbstractVariable): Unit = {
    columns(v) -= subject
  }

  protected def chooseSubject(expr: LinearExpression): (Option[AbstractVariable], LinearExpression) = {
    var foundUnrestricted = false
    var foundNewRestricted = false
    var subject: Option[AbstractVariable] = Option.empty
    var continueSearch = true
    for (v <- expr.terms.keys; c = expr.terms(v) if (continueSearch)) {
      if (foundUnrestricted) {
        if (!v.isRestricted) {
          if (!this.columns.contains(v)) {
            subject = Some(v)
            continueSearch = false
          }
        }
      } else { // we haven't found a restricted variable yet
        if (v.isRestricted) {
          if (!foundNewRestricted && !v.isDummy && c < 0.0) {
            val col = this.columns.get(v)
            if (!col.isDefined || (col.get.size == 1 && this.columns.contains(objective))) {
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
          if (!this.columns.contains(v)) {
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

  final def setExternalVariables() = {
    //    externalParametricVars foreach { v =>
    //      require(rows.get(v).isDefined, "variable $v in externalParametricVars is basic")
    //      v.value = 0
    //    }
    //    externalRows foreach { v =>
    //      val expr = rows(v)
    //      v.value = expr.constant
    //    }
  }

  protected def newExpression(cn: Constraint, objective: ObjectiveVariable) = {
    val eplus_eminus: mutable.Buffer[SlackVariable] = mutable.Buffer.empty
    val cnExpr = cn.expression
    var expr = new LinearExpression(cnExpr.constant) //<< fixme: rework to val instead of var
    var prevEConstant = 0.0;
    cnExpr.terms.keys foreach { v =>
      val c = cnExpr.terms(v)
      val e = rows.get(v)
      expr = if (!e.isDefined) {
        expr.addVariable(v, c)
      } else {
        expr.addExpression(e.get, c)
      }
    }
    if (cn.isInstanceOf[LinearInequality]) {
      val slackVar = variableFactory.newSlackVariable()
      expr = expr.setVariable(slackVar, -1)
      markerVars.put(cn, slackVar)
      if (!cn.isRequired) {
        val eminus = variableFactory.newSlackVariable()
        expr = expr.setVariable(eminus, 1.0)
        val zRow = rows(objective)
        val sw = cn.strength.symbolicWeight * cn.weight
        rows += objective -> zRow.setVariable(eminus, sw)
        insertErrorVar(cn, eminus)
        noteAddedVariable(eminus, objective);
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
        val zRow = rows(objective)
        val sw = cn.strength.symbolicWeight * cn.weight
        rows += objective -> zRow.setVariable(eplus, sw)
        noteAddedVariable(eplus, objective);
        rows += objective -> zRow.setVariable(eminus, sw)
        noteAddedVariable(eminus, objective);
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

  /** Minimize the value of the objective. (The tableau should already be feasible.) */
  final def optimize(zVar: ObjectiveVariable) = {
    val zRow = rows(zVar)
    var objectiveCoeff = 0.0
    var entryVar: Option[AbstractVariable] = None
    var exitVar: Option[AbstractVariable] = None
    def findNextEntryVar = {
      val terms = zRow.terms
      terms.keys foreach { v =>
        val c = terms(v)
        if (v.isPivotable && c < objectiveCoeff) {
          objectiveCoeff = c
          entryVar = Some(v)
        }
      }
      objectiveCoeff >= -AbstractVariable.epsilon || entryVar.isEmpty
    }
    while (findNextEntryVar) {
      var minRatio = Double.MaxValue
      val columnVars = columns(entryVar.get)
      var r = 0.0
      columnVars foreach { v =>
        if (v.isPivotable) {
          val expr = rows(v)
          val coeff = expr.coefficientFor(v)
          if (coeff < 0.0) {
            r = -expr.constant / coeff
            if (r < minRatio) {
              minRatio = r
              exitVar = Some(v)
            }
          }
        }
      }
      require(minRatio != Double.MaxValue, "Objective function is unbounded in optimize")
      require(entryVar.isDefined && exitVar.isDefined)
      pivot(entryVar.get, exitVar.get)
    }
  }

  protected def pivot(entryVar: AbstractVariable, exitVar: AbstractVariable) = {
    val pexpr = removeRow(exitVar)
    val pexpr2 = pexpr.changeSubject(exitVar, entryVar)
    addRow(entryVar, pexpr2)
  }

  protected def insertColumns(terms: scala.collection.Set[AbstractVariable], rowVariable: AbstractVariable): Unit = {
    terms foreach {
      term =>
        columns += (term -> (columns.getOrElse(term, mutable.Set.empty) + rowVariable))
        if (term.isExternal) externalParametricVars += term.asInstanceOf[Variable]
    }
  }

  protected def addRow(v: AbstractVariable, expr: LinearExpression): Unit = {
    rows += (v -> expr)
    insertColumns(expr.terms.keySet, v)
    if (v.isExternal) externalRows += v.asInstanceOf[Variable]
  }

  protected def removeColumn(v: AbstractVariable): Unit = {
    columns -= v
    if (v.isExternal) {
      externalRows -= v.asInstanceOf[Variable]
      externalParametricVars -= v.asInstanceOf[Variable]
    }
  }

  /** Remove the basic variable v from the tableau row v=expr, then update column cross indices */
  protected def removeRow(v: AbstractVariable): LinearExpression = {
    val expr = rows(v)
    expr.terms.keys foreach { v =>
      val varset = columns.get(v)
      if (varset.isDefined) varset.get -= v
    }
    infeasibleRows -= v
    if (v.isExternal) externalRows -= v.asInstanceOf[Variable]
    rows -= v
    expr
  }

  /**
   * Replace all occurrences of oldVar with expr, and update column cross
   * indices, oldVar should now be a basic variable
   */
  protected def substitueOut(oldVar: AbstractVariable, expr: LinearExpression): Unit = {
    columns(oldVar) foreach { v =>
      val row = rows(v)
      val revisedRow = substituteOut(row, oldVar, expr, v)
      rows(v) = revisedRow
      if (v.isRestricted && row.constant < 0.0) {
        infeasibleRows += v
      }
    }
    if (oldVar.isExternal) {
      externalRows += oldVar.asInstanceOf[Variable]
      externalParametricVars -= oldVar.asInstanceOf[Variable]
    }
  }

  protected def substituteOut( // Note: removed from LinearExpression to this class
    expr1: LinearExpression, variable: AbstractVariable,
    expr2: LinearExpression, subject: AbstractVariable): LinearExpression = {
    val multiplier = expr1.terms(variable)
    val revisedTerms1 = expr1.terms - variable
    val revisedConstant = multiplier * expr2.constant
    val termsToRevise: mutable.Map[AbstractVariable, Double] = mutable.Map.empty
    val termsToRemove = mutable.Buffer[AbstractVariable]()
    expr2.terms.keys foreach { clv =>
      val coeff = expr2.terms(clv)
      val dOldCoeff = expr1.terms.get(clv)
      if (dOldCoeff.isDefined) {
        val oldCoeff = dOldCoeff.get
        val newCoeff = oldCoeff + multiplier * coeff
        if (Math.abs(newCoeff) < AbstractVariable.epsilon) {
          noteRemovedVariable(clv, subject)
          termsToRemove += clv
        } else {
          termsToRevise += (clv -> newCoeff)
        }
      } else { // did not have that variable already
        termsToRevise += (clv -> multiplier * coeff)
        noteAddedVariable(clv, subject)
      }
    }
    val revisedTerms = revisedTerms1 -- termsToRemove ++ termsToRevise
    LinearExpression(revisedTerms, revisedConstant)
  }

  protected def substituteOut(oldVar: AbstractVariable, expr: LinearExpression): Unit = {
    val varset = columns(oldVar)
    varset foreach { v =>
      val row = rows(v)
      val revisedRow = substituteOut(row, oldVar, expr, v)
      rows(v) = revisedRow
      if (v.isRestricted && revisedRow.constant < 0.0) {
        this.infeasibleRows += v
      }
    }
    if (oldVar.isExternal) {
      externalRows += oldVar.asInstanceOf[Variable]
      externalParametricVars -= oldVar.asInstanceOf[Variable]
    }
    columns -= oldVar
  }

  protected def resetStayConstants(): Unit = {
    stayPlusErrorVars zip stayMinusErrorVars foreach { v =>
      val expr1 = rows.get(v._1)
      if (expr1.isDefined) {
        rows(v._1) = LinearExpression(expr1.get.terms, 0)
      } else {
        val expr2 = rows.get(v._2)
        if (expr2.isDefined) {
          rows(v._1) = LinearExpression(expr2.get.terms, 0)
        }
      }
    }
  }

  protected def noteAddedVariable(v: AbstractVariable, subject: AbstractVariable): Unit = {
    columns += (v -> (columns.getOrElse(v, mutable.Set.empty) + subject))
  }

  protected def insertErrorVar(cn: Constraint, v: AbstractVariable) = {
    val cnset = errorVars.get(cn)
    val cnset2 = (if (cnset.isDefined) cnset.get else new mutable.HashSet[AbstractVariable]) + v
    errorVars += cn -> cnset2
  }
}