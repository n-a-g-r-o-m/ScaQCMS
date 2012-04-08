/*
 * ScalaQCMS -- Scala Quantum Circuit Model Simulator
 *
 * Copyright (c) 2012 Antti Vikman
 */

package models

import solvingpolicies.{DummySolvingPolicy, SolvingPolicy}

object  SolvingPolicies {
  private val _solvingPolicies = Map[String, SolvingPolicy] (
    "dummy" -> DummySolvingPolicy
  )

  def getSolvingPolicies = _solvingPolicies.keySet
  
  def solve(equation: EquationEntity,
            solvingPolicy: String = "dummy",
            solvers: Set[String] = Solvers.getSolvers): EquationEntity = {
    if(equation == null)
      throw new NullPointerException("Variable equation can not be null")
    if(!_solvingPolicies.contains(solvingPolicy))
      throw new MatchError("No such solving policy as \"" + solvingPolicy + "\"")
    _solvingPolicies.get(solvingPolicy).get.solve(equation, solvers)
  }
}
