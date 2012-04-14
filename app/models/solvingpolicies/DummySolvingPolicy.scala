/*
 * ScalaQCMS -- Scala Quantum Circuit Model Simulator
 *
 * Copyright (c) 2012 Antti Vikman
 */

package models.solvingpolicies

import models._
import scala.actors.Futures._

/**
 * Dummy solving policy that supports parallel execution and finding of the "fastest" algorithm to solve current
 * equation. This is implemented using async recursive execution of the policy itself, so solvers doesn't need to
 * be parallel in order to get some parallel execution done.
 */
object DummySolvingPolicy extends SolvingPolicy {
  def solve(equation: EquationEntity, solvers: Set[String]): EquationEntity = {
    if (equation.isInstanceOf[EquationSymbol])
      equation
    else {
      val _equation = equation.asInstanceOf[Equation]
      val solverOfA = future { solve(_equation.A, solvers) }  // Let's start async execution of solving the A (left) part of the equation
      val solverOfB = future { solve(_equation.B, solvers) }  // and the same for the B (right) part
      _equation.A = solverOfA.inputChannel.? // Then we get the result of A (right) part
      _equation.B = solverOfB.inputChannel.? // and B (left) part
                                             // and move to solve this equation

      // This policy is a _Dummy_ policy, so it always uses the "fastest" solver, no matter how overloaded it is
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
      _equation.result = Solvers.solve(_equation, fastest).getResult // And we use "the chosen" solver to solve this equation
      _equation
    }
  }
}
