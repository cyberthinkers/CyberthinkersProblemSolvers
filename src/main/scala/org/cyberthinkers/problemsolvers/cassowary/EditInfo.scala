package org.cyberthinkers.problemsolvers.cassowary

case class EditInfo(
    constraint: Constraint,
    eplus: SlackVariable,
    eminus: SlackVariable,
    prefEditConstant: Double,
    index: Int) {
}