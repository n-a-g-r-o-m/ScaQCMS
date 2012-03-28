/*
 * ScalaQCMS -- Scala Quantum Circuit Model Simulator
 *
 * Copyright (c) 2012 Antti Vikman
 */

package models.solvers

import models.EquationEntity

case class SolverStatistics(tensoringTimePerCell: Long, multiplyingTimePerCell: Long)

abstract class Solver {
  //TODO: I Guess trait would be more correct
  var totalTensoringTime, totalMultiplyTime, totalTensoringCells, totalMultiplyCells = 0L

  def call(equation: EquationEntity): EquationEntity

  def statistics: SolverStatistics = {
    val tensoringTimePerCell = (if (totalTensoringCells == 0) 0L else totalTensoringTime / totalTensoringCells)
    val multiplyingTimePerCell = (if (totalMultiplyCells == 0) 0L else totalMultiplyTime / totalMultiplyCells)
    new SolverStatistics(tensoringTimePerCell, multiplyingTimePerCell)
  }
}
