/*
 * ScalaQCMS -- Scala Quantum Circuit Model Simulator
 *
 * Copyright (c) 2012 Antti Vikman
 */

package models.solvers

import scalala.library.LinearAlgebra._
import scalala.scalar.Complex

import models.{EquationSymbol, EquationOperator, Equation, EquationEntity}

/**
 * DummySolver is very simple implementation of solver, that relies fully on the arithmetic implementation
 * of scalala in matrix multiplication and tensoring.
 */
object DummySolver extends Solver {
  def solve(equation: EquationEntity) = {
    if (equation.isInstanceOf[Equation]) {
      val _equation: Equation = equation.asInstanceOf[Equation]
      val timer = new models.utils.Timer
      timer.start()
      val resultEquation = {
        _equation.operator match {
          case EquationOperator.Multiply =>
            new EquationSymbol(_equation.toString, _equation.A.getResult * _equation.B.getResult)
          case EquationOperator.Kronecker =>
            new EquationSymbol(_equation.toString, kron[Complex, Complex, Complex](_equation.A.getResult,
              _equation.B.getResult))
        }
      }
      val time = timer.stop
      val cells = _equation.size
      _equation.operator match {
        case EquationOperator.Multiply =>
          totalMultiplyCells += cells
          totalMultiplyTime += time
        case EquationOperator.Kronecker =>
          totalTensoringCells += cells
          totalTensoringTime += time
      }
      resultEquation
    } else
      equation
  }
}
