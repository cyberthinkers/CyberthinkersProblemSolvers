package org.cyberthinkers.problemsolvers.cassowary

class VariableFactory(var nextIndex:Integer = 0) { // assumes single thread here
  def newDummyVariable() = {nextIndex += 1 ; new DummyVariable(nextIndex)}
  def newObjectiveVariable() = {nextIndex += 1 ; new ObjectiveVariable(nextIndex)}
  def newSlackVariable() = {nextIndex += 1 ; new SlackVariable(nextIndex)}
  def newVariable(value: Double) = {nextIndex += 1 ; new Variable(value, nextIndex)}
}