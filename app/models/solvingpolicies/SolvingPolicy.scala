/*
 * ScalaQCMS -- Scala Quantum Circuit Model Simulator
 *
 * Copyright (c) 2012 Antti Vikman
 */

package models.solvingpolicies

import models.EquationEntity

/**
 * SolvingPolicy is an abstract class to provide unified interface for all solving policies
 * @todo Looks like this could be refactored to Trait
 */
abstract class SolvingPolicy {
  /**
   * Solves the equation using the given set of solvers
   * @param equation is the equation to  be solved
   * @param solvers is a set of Solver names that can be used in solving
   * @return returns the solved equation
   */
  def solve(equation: EquationEntity, solvers: Set[String]): EquationEntity
}
