package org.cyberthinkers.problemsolvers.cassowary

import scala.collection.mutable

class Tableau(
    val columns: mutable.Map[AbstractVariable, mutable.Set[AbstractVariable]] = mutable.Map.empty,
    val rows: mutable.Map[AbstractVariable, LinearExpression] = mutable.Map.empty,
    val infeasibleRows: mutable.Set[AbstractVariable] = mutable.Set.empty,
    val externalRows: mutable.Set[AbstractVariable] = mutable.Set.empty,
    val externalParametricVars: mutable.Set[AbstractVariable] = mutable.Set.empty) {

  // FIXME: check should be made that the subject is in the tableau
  protected def noteRemovedVariable(v: AbstractVariable, subject: AbstractVariable): Unit = {
    columns(v).remove(subject)
  }

  def noteAddedVariable(v: AbstractVariable, subject: AbstractVariable): Unit = {
    columns += (v -> (columns.getOrElse(v, mutable.Set.empty) + subject))
  }

  private def insertColumns(terms: scala.collection.Set[AbstractVariable], rowVariable: AbstractVariable): Unit = {
    terms foreach {
      term =>
        columns += (term -> (columns.getOrElse(term, mutable.Set.empty) + rowVariable))
        if (term.isExternal) externalParametricVars += term
    }
  }

  def addRow(v: AbstractVariable, expr: LinearExpression): Unit = {
    rows += (v -> expr)
    insertColumns(expr.terms.keySet, v)
    if (v.isExternal) externalRows += v
  }

  protected def removeColumn(v: AbstractVariable): Unit = {
    columns -= v
    if (v.isExternal) {
      externalRows -= v
      externalParametricVars -= v
    }
  }

  /** Remove the basic variable v from the tableau row v=expr, then update column cross indices */
  def removeRow(v: AbstractVariable): LinearExpression = {
    val expr = rows(v)
    expr.terms.keys foreach { v =>
      val varset = columns.get(v)
      if (varset.isDefined) varset.get -= v
    }
    infeasibleRows -= v
    if (v.isExternal) externalRows -= v
    rows -= v
    expr
  }

  /**
   * Replace all occurrences of oldVar with expr, and update column cross
   * indices, oldVar should now be a basic variable
   */
  def substitueOut(oldVar: AbstractVariable, expr: LinearExpression): Unit = {
    columns(oldVar) foreach { v =>
      val row = rows(v)
      val revisedRow = substituteOut(row, oldVar, expr, v)
      rows(v) = revisedRow
      if (v.isRestricted && row.constant < 0.0) {
        infeasibleRows += v
      }
    }
    if (oldVar.isExternal) {
      externalRows += oldVar
      externalParametricVars -= oldVar
    }
  }

  private[this] def substituteOut( // Note: removed from LinearExpression to this class
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
      externalRows += oldVar
      externalParametricVars -= oldVar
    }
    columns -= oldVar
  }
}