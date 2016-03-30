package org.cyberthinkers.problemsolvers.cassowary

import scala.collection.mutable

class Tableau(
    val columns: mutable.Map[AbstractVariable, mutable.Set[AbstractVariable]] = mutable.Map.empty,
    val rows: mutable.Map[AbstractVariable, LinearExpression] = mutable.Map.empty,
    val infeasibleRows: mutable.Set[AbstractVariable] = mutable.Set.empty,
    val externalRows: mutable.Set[AbstractVariable] = mutable.Set.empty,
    val externalParametricVars: mutable.Set[AbstractVariable] = mutable.Set.empty) {

  // check should be made that the subject is in the tableau
  protected def noteRemovedVariable(v: AbstractVariable, subject: AbstractVariable): Unit = {
    columns(v).remove(subject)
  }

  protected def noteAddedVariable(variable: AbstractVariable, subject: AbstractVariable): Unit = {
    columns += (variable -> (columns.getOrElse(variable, mutable.Set.empty) + subject))
  }

  private def insertColumns(terms: scala.collection.Set[AbstractVariable] , rowVariable: AbstractVariable): Unit = {
    terms foreach {
      term =>
        columns += (term -> (columns.getOrElse(term, mutable.Set.empty) + rowVariable))
        if (term.isExternal) externalParametricVars += term
    }
  }

  protected def addRow(variable: AbstractVariable, expr: LinearExpression): Unit = {
    rows += (variable -> expr)
    insertColumns(expr.terms.keySet, variable)
    if (variable.isExternal) externalRows += variable
  }

  protected def removeColumn(variable: AbstractVariable): Unit = {
    columns -= variable
    if (variable.isExternal) {
      externalRows -= variable
      externalParametricVars -= variable
    }
  }

  protected def removeRow(variable: AbstractVariable): Unit = {
    val expr = rows(variable)
    expr.terms.keys foreach {
      v =>
        val varset = columns.get(v)
        if (varset.isDefined) varset.get.remove(v)
    }
    if (variable.isExternal) externalRows -= variable
    rows.remove(variable)
    infeasibleRows -= variable
    if (variable.isExternal) externalRows.remove(variable)
    rows.remove(variable)
  }

  private def substituteOut(
    expr1: LinearExpression, variable: AbstractVariable,
    expr2: LinearExpression, subject: AbstractVariable) = {
    def isApproxZero(value: Double): Boolean = Math.abs(value) < AbstractVariable.epsilon
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
        if (isApproxZero(newCoeff)) {
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

  protected def substitueOut(oldVar: AbstractVariable, expr: LinearExpression) = {
    val varset = columns(oldVar)
    varset foreach { v =>
      val row = rows(v)
      val revisedRow = substituteOut(row, oldVar, expr, v)
      if (v.isRestricted && row.constant < 0.0) {
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