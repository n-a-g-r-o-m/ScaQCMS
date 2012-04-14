/*
 * ScalaQCMS -- Scala Quantum Circuit Model Simulator
 *
 * Copyright (c) 2012 Antti Vikman
 */

package models.solvers

import models.EquationEntity

/**
 * Container of execution statistics of pre processor.
 * @param tensoringTimePerCell microseconds per cell tensoring on this Solver has took in average
 * @param multiplyingTimePerCell microseconds per cell multiplying on this Solver has took in average
 * @todo We should add data conversion times, (un)packing times, data transfer times, optimal matrix size etc.
 *       to make this really useful for solving policies to find the fastest and most suitable.
 */
case class SolverStatistics(tensoringTimePerCell: Long, multiplyingTimePerCell: Long)

/**
 * Solver is an abstract class to provide unified interface for all the solvers
 */
abstract class Solver {
  var totalTensoringTime, totalMultiplyTime, totalTensoringCells, totalMultiplyCells = 0L

  /**
   * Solves the given equation
   * @param equation the equation to be solved
   * @return equation with a result to the equation given as a parameter
   */
  def solve(equation: EquationEntity): EquationEntity

  /**
   * Returns the statistics of this Solver. This data can be consumed by the SolvingPolicy
   * when choosing suitable solver(s).
   * @return SolverStatistics that describes the execution statistics of previous executions
   */
  def statistics: SolverStatistics = {
    val tensoringTimePerCell = (if (totalTensoringCells == 0) 0L else totalTensoringTime / totalTensoringCells)
    val multiplyingTimePerCell = (if (totalMultiplyCells == 0) 0L else totalMultiplyTime / totalMultiplyCells)
    new SolverStatistics(tensoringTimePerCell, multiplyingTimePerCell)
  }
}
