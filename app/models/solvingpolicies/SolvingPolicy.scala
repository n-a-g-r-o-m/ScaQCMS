/*
 * ScalaQCMS -- Scala Quantum Circuit Model Simulator
 *
 * Copyright (c) 2012 Antti Vikman
 */

package models.solvingpolicies

import models.EquationEntity

abstract class SolvingPolicy {
  def solve(equation: EquationEntity, solvers: Set[String]): EquationEntity
}
