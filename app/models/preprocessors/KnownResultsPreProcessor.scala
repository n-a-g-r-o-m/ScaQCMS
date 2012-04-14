/*
 * ScalaQCMS -- Scala Quantum Circuit Model Simulator
 *
 * Copyright (c) 2012 Antti Vikman
 */

package models.preprocessors

import models.{EquationOperator, Equation, EquationEntity}

/**
 * KnownResultsPreProcessor replaces equations that result is known, with the result.
 * This simply removes the need to spend solving power to find an answer we already know.
 */
object KnownResultsPreProcessor extends PreProcessor {

  /**
   * Replaces equations with known result with the result. This function should be called via process.
   * Currently removes the multiplications where one of the side is "i" (identity matrix),
   * since multiplying anything with identity matrix simply returns what was multiplied.
   *
   * @param equation the equation to be processed
   * @return an equation where known results has replaced the equations
   */
  private def removeKnownResults(equation: EquationEntity): EquationEntity = {
    if (equation.isInstanceOf[Equation]) {
      var _equation = equation.asInstanceOf[Equation]
      _equation.A = removeKnownResults(_equation.A) // This could be parallelled, but it takes so little time
      _equation.B = removeKnownResults(_equation.B) // that there is no much use of it.
      if (_equation.operator == EquationOperator.Multiply)
        if(_equation.A.toString == "i")
          return _equation.B
        else if(_equation.B.toString == "i")
          return _equation.A
    }
    equation
  }

  def process(equation: EquationEntity) = {
    val timer = new models.utils.Timer
    timer.start()
    val resultEquation = removeKnownResults(equation)
    totalTime += timer.stop
    times += 1
    resultEquation
  }
}
