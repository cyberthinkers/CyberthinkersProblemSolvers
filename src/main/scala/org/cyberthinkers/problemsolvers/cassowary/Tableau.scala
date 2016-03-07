package org.cyberthinkers.problemsolvers.cassowary

class Tableau(
    val columns: Map[AbstractVariable, Set[AbstractVariable]] = Map.empty,
    val rows: Map[AbstractVariable, LinearExpression] = Map.empty,
    val infeasibleRows: Set[AbstractVariable] = Set.empty, 
    val externalRows: Set[AbstractVariable] = Set.empty,
    val externalParametricVars: Set[AbstractVariable] = Set.empty) {

  protected def noteRemovedVariable(variable: AbstractVariable , subject: AbstractVariable) = {
    val revisedColumns = columns -- columns.get(variable).get
    new Tableau(revisedColumns, rows, infeasibleRows, externalRows, externalParametricVars)
  }
  
  protected def noteAddedVariable(variable: AbstractVariable , subject: AbstractVariable) = {
    val rowset = variable -> (columns.getOrElse(variable, Set.empty) + subject)
    val revisedColumns = columns + rowset
    new Tableau(revisedColumns, rows, infeasibleRows, externalRows, externalParametricVars)
  }
 
  protected def addRow(variable: AbstractVariable, expr: LinearExpression): Tableau = {
    val revisedColumns = insertColumns(expr.terms.keySet, variable)
    val revisedRows = rows + (variable -> expr)
    val revisedExternalRows = if (variable.isExternal) externalRows + variable else externalRows
    val revisedExternalParametricVars = externalParametricVars ++ expr.terms.keys.filter(_.isExternal)
    new Tableau(revisedColumns, revisedRows, infeasibleRows, revisedExternalRows, revisedExternalParametricVars)
  }
  
  protected def removeColumn(variable: AbstractVariable) = {
    val rowsRemoved = columns.get(variable)
    val revisedColumns = columns - variable
    //FIXME -- still working on this
  }
  
  private def insertColumns(terms: Set[AbstractVariable], rowVariable: AbstractVariable) = {
    val v1 = for {
      term <- terms
      rowset = term -> (columns.getOrElse(term, Set.empty) + rowVariable)
    } yield rowset
    columns ++ v1.toMap //FIXME- rework to get rid of toMap, which is an extra step
  }

  protected def removeRow(variable: AbstractVariable): Tableau = {
    val expr = rows.get(variable)
    val revisedColumns = for {
      (k, v) <- expr.get.terms
      varset = columns.get(k)
      if(!varset.isDefined)
      revisedVarset = varset.get - variable
    } yield (k -> revisedVarset)
    val revisedInfeasibleRows = this.infeasibleRows - variable
    val revisedExternalRows = if(variable.isExternal) externalRows - variable else externalRows
    val revisedRows = rows - variable
    new Tableau(revisedColumns, revisedRows, infeasibleRows, revisedExternalRows, externalParametricVars)
  }
  
  protected def substitueOut(oldVar: AbstractVariable, expr: LinearExpression) {
//    class RichMap[A, B](m: Map[A, B]) {
//      def thatIntersectWith(ks: Set[A]) = ks.flatMap(k => m.get(k).map((k, _))).toMap
//      def thatIntersectWith(ks: A*) = ks.flatMap(k => m.get(k).map((k, _))).toMap
////    interestingKeys.flatMap(k => originalMap.get(k).map((k, _))).toMap
//    }
//    implicit def enrichMap[A, B](m: Map[A, B]) = new RichMap(m)
    val varset = columns.get(oldVar).get
    val c = varset collect this.columns
 
   // >>> more stuff goes here
    // mod infeasible rows from v
  }
}