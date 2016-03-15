package org.cyberthinkers.problemsolvers.cassowary

class Tableau(
    val columns: Map[AbstractVariable, Set[AbstractVariable]] = Map.empty,
    val rows: Map[AbstractVariable, LinearExpression] = Map.empty,
    val infeasibleRows: Set[AbstractVariable] = Set.empty,
    val externalRows: Set[AbstractVariable] = Set.empty,
    val externalParametricVars: Set[AbstractVariable] = Set.empty) {

  // check should be made before calling this that the subject is in the tableau
  protected def noteRemovedVariable(variable: AbstractVariable, subject:AbstractVariable): Tableau = {
    val r = columns(variable) - subject
    val revisedColumns = columns + (variable -> r)
    new Tableau(revisedColumns, rows, infeasibleRows, externalRows, externalParametricVars)
  }

  protected def noteAddedVariable(variable: AbstractVariable, subject: AbstractVariable): Tableau = {
    val revisedColumns = columns + (variable -> (columns.getOrElse(variable, Set.empty) + subject))
    new Tableau(revisedColumns, rows, infeasibleRows, externalRows, externalParametricVars)
  }

  private def insertColumns(terms: Set[AbstractVariable], rowVariable: AbstractVariable) = {
    val revisedColumns = scala.collection.mutable.HashMap.empty[AbstractVariable, Set[AbstractVariable]]
    val externalTerms = scala.collection.mutable.HashSet.empty[AbstractVariable]
    terms foreach {
      term =>
        revisedColumns += (term -> (columns.getOrElse(term, Set.empty) + rowVariable))
        if (term.isExternal) externalTerms += term
    }
    (columns ++ revisedColumns, externalParametricVars ++ externalTerms)
  }

  protected def addRow(variable: AbstractVariable, expr: LinearExpression): Tableau = {
    val revisedRows = rows + (variable -> expr)
    val (revisedColumns, revisedExternalParametricVars) = insertColumns(expr.terms.keySet, variable)
    val revisedExternalRows = if (variable.isExternal) externalRows + variable else externalRows
    new Tableau(revisedColumns, revisedRows, infeasibleRows, revisedExternalRows, revisedExternalParametricVars)
  }

  protected def removeColumn(variable: AbstractVariable): Tableau = {
    val revisedColumns = columns - variable
    val (revisedExternalRows, revisedExternalParametricVars) =
      if (variable.isExternal) (externalRows - variable, externalParametricVars - variable)
      else (externalRows, externalParametricVars)
    new Tableau(revisedColumns, rows, infeasibleRows, revisedExternalRows, revisedExternalParametricVars)
  }

  protected def removeRow(variable: AbstractVariable): Tableau = {
    val revisedInfeasibleRows = infeasibleRows - variable
    val revisedExternalRows = if (variable.isExternal) externalRows - variable else externalRows
    val revisedRows = rows - variable
    new Tableau(columns, revisedRows, revisedInfeasibleRows, revisedExternalRows, externalParametricVars)
  }
  
  private case class Removed(variable: AbstractVariable, expr: LinearExpression)
  
  private def substituteOut(
      expr1: LinearExpression, variable: AbstractVariable,
      expr2: LinearExpression, subject: AbstractVariable) = {
    val multiplier = expr1.terms(variable)
    val revisedTerms = expr1.terms - variable
    val revisedConstant = multiplier * expr2.constant
    
    expr2.terms.keys foreach { clv =>
      val coeff = expr2.terms(clv)
      val dOldCoeff = expr1.terms.get(clv)
      if(dOldCoeff.isDefined) {
        val oldCoeff = dOldCoeff.get
        val newCoeff = oldCoeff + multiplier * coeff
        
      } else {
        
      }
    }
  }
  
  protected def substitueOut(oldVar: AbstractVariable, expr: LinearExpression): Tableau = {
    val infeasibleRowsTmp = scala.collection.mutable.HashSet.empty[AbstractVariable]
    val terms = columns(oldVar)
    terms foreach { term =>
      val r = rows(term)
      // substitueOut...
      if (term.isRestricted && r.constant < 0) {
        infeasibleRowsTmp += term
      }
    }
    val revisedInfeasibleRows = infeasibleRows ++ infeasibleRowsTmp
    val (revisedExternalRows, revisedExternalParametricVars) =
      if (oldVar.isExternal) (externalRows + oldVar, externalParametricVars - oldVar) else (externalRows, externalParametricVars)
    new Tableau(columns, rows, revisedInfeasibleRows, revisedExternalRows, externalParametricVars)
  }
}