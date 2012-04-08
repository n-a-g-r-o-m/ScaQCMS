/*
 * ScalaQCMS -- Scala Quantum Circuit Model Simulator
 *
 * Copyright (c) 2012 Antti Vikman
 */

package models.preprocessors

import models.{EquationOperator, Equation, EquationEntity}

object RotationPreProcessor extends PreProcessor {
  private def rotate(equation: EquationEntity): EquationEntity = {
    if (equation.isInstanceOf[Equation]) {
      val _equation = equation.asInstanceOf[Equation]
      _equation.B = rotate(_equation.B)
      _equation.A = rotate(_equation.A)
      if (_equation.operator == EquationOperator.Multiply)
        if (_equation.A.isInstanceOf[Equation])
          if (_equation.B.isInstanceOf[Equation]) {
            val _A = _equation.A.asInstanceOf[Equation]
            val _B = _equation.B.asInstanceOf[Equation]
            if (_A.operator == EquationOperator.Kronecker)
              if (_B.operator == EquationOperator.Kronecker) {
                if (_A.A.cols == _B.A.rows)
                  if (_A.B.cols == _B.B.rows) {
                    val store = _A.B
                    _A.B = _B.A
                    _A.operator = EquationOperator.Multiply
                    _B.A = store
                    _B.operator = EquationOperator.Multiply
                    _equation.A = _A
                    _equation.B = _B
                    _equation.operator = EquationOperator.Kronecker
                  }

              }

          }
      _equation
    }
    equation
  }
  def call(equation: EquationEntity) = {
    val timer = new models.utils.Timer
    timer.start
    val resultEquation = rotate(equation)
    totalTime += timer.stop
    times += 1
    resultEquation
  }
}
