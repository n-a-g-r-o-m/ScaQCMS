/*
 * ScalaQCMS -- Scala Quantum Circuit Model Simulator
 *
 * Copyright (c) 2012 Antti Vikman
 */

package models.preprocessors

import models.{EquationOperator, Equation, EquationEntity}

/**
 * OperatorSwapPreProcessor swaps solving order of tensoring and multiplication, when ever it's possible.
 * We know that (A⊗B)*(C⊗D) equals (A*C)⊗(B*D), when A.cols equals C.rows and B.cols equals D.rows.
 * If all matrices A-D are 2x2 times to matrices, in first case we would have two times tensoring of 2x2 matrices and
 * once multiplication of 4x4 matrices, which contains way much more arbitrary operations than the second case where
 * we have two times multiplication of 2x2 matrices and once tensoring of 4x4 matrices.
 */
object OperatorSwapPreProcessor extends PreProcessor {
  /**
   * Swaps the tensoring operation and multiplication operations (when possible). This should be called via process.
   * @param equation the equation to be processed
   * @return an equation where tensoring operation and multiplication operations has been swapped
   */
  private def swap(equation: EquationEntity): EquationEntity = {
    if (equation.isInstanceOf[Equation]) {
      val _equation = equation.asInstanceOf[Equation]
      _equation.B = swap(_equation.B)
      _equation.A = swap(_equation.A)
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

  def process(equation: EquationEntity) = {
    val timer = new models.utils.Timer
    timer.start()
    val resultEquation = swap(equation)
    totalTime += timer.stop
    times += 1
    resultEquation
  }
}
