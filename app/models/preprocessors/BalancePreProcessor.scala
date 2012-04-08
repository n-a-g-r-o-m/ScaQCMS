/*
 * ScalaQCMS -- Scala Quantum Circuit Model Simulator
 *
 * Copyright (c) 2012 Antti Vikman
 */

package models.preprocessors

import models.{EquationOperator, Equation, EquationEntity}

object BalancePreProcessor extends PreProcessor {

  private def balance(equation: EquationEntity): EquationEntity = {
    if(equation.isInstanceOf[Equation]) {
      val _equation = equation.asInstanceOf[Equation]
      _equation.B = balance(_equation.B)
      if (_equation.A.getStatistics.depth < (_equation.B.getStatistics.depth - 1)) {
        val _B = _equation.B.asInstanceOf[Equation]
        if (_equation.operator == EquationOperator.Multiply) {
          if (_B.operator == EquationOperator.Multiply) {
            if (_equation.A.cols == _B.A.rows) {
              _equation.A = new Equation(_equation.A, _B.A, EquationOperator.Multiply)
              _equation.B = _B.B
            }
          }
        }
        else if (_equation.operator == EquationOperator.Kronecker) {
          if (_B.operator == EquationOperator.Kronecker) {
            _equation.A = new Equation(_equation.A, _B.A, EquationOperator.Kronecker)
            _equation.B = _B.B
          }
        }
      } else if ((_equation.A.getStatistics.depth - 1) > _equation.B.getStatistics.depth - 1) {
        val _A = _equation.A.asInstanceOf[Equation]
        if (_equation.operator == EquationOperator.Multiply) {
          if (_A.operator == EquationOperator.Multiply) {
            if (_A.B.cols == _equation.A.rows) {
              _equation.B = new Equation(_A.B, _equation.B, EquationOperator.Multiply)
              _equation.A = _A.A
            }
          }
        }
        else if (_equation.operator == EquationOperator.Kronecker) {
          if (_A.operator == EquationOperator.Kronecker) {
            _equation.B = new Equation(_A.B, _equation.B, EquationOperator.Kronecker)
            _equation.A = _A.A
          }
        }
      }
      _equation.A = balance(_equation.A)
      _equation
    }
    equation
  }

  def call(equation: EquationEntity) = {
    val timer = new models.utils.Timer
    timer.start
    val resultEquation = balance(equation)
    totalTime += timer.stop
    times += 1
    resultEquation
  }
}