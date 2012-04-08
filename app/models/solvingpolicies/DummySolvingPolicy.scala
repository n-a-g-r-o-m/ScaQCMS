/*
 * ScalaQCMS -- Scala Quantum Circuit Model Simulator
 *
 * Copyright (c) 2012 Antti Vikman
 */

package models.solvingpolicies

import models._
import scala.actors.Futures._

object DummySolvingPolicy extends SolvingPolicy {
  def solve(equation: EquationEntity, solvers: Set[String]): EquationEntity = {
    if (equation.isInstanceOf[EquationSymbol])
      equation
    else {
      val _equation = equation.asInstanceOf[Equation]
      val solverOfA = future { solve(_equation.A, solvers) }
      val solverOfB = future { solve(_equation.B, solvers) }
      _equation.A = solverOfA.inputChannel.?
      _equation.B = solverOfB.inputChannel.?

      var fastest: String = ""
      var fastestsTime = Long.MaxValue
      for ((name, stats) <- Solvers.getStatsOfSolvers(solvers)) {
        _equation.operator match {
          case EquationOperator.Multiply =>
            if (fastestsTime > stats.multiplyingTimePerCell) {
              fastest = name
              fastestsTime = stats.multiplyingTimePerCell
            }
          case EquationOperator.Kronecker =>
            if (fastestsTime > stats.tensoringTimePerCell) {
              fastest = name
              fastestsTime = stats.tensoringTimePerCell
            }
        }
      }
      _equation.result = Solvers.solve(_equation, fastest).getResult
      _equation
    }
  }
}
