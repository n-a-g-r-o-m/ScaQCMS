/*
 * ScalaQCMS -- Scala Quantum Circuit Model Simulator
 *
 * Copyright (c) 2012 Antti Vikman
 */

package models.preprocessors

import models.{EquationOperator, Equation, EquationEntity}

/**
 * Created by n-a-g-r-o-m
 * 4/8/12 7:38 AM
 * Copyright (c) 2012
 */

object KnownResultsPreProcessor extends PreProcessor {
  private def removeKnownResults(equation: EquationEntity): EquationEntity = {
    if (equation.isInstanceOf[Equation]) {
      var _equation = equation.asInstanceOf[Equation]
      _equation.A = removeKnownResults(_equation.A)
      _equation.B = removeKnownResults(_equation.B)
      if (_equation.operator == EquationOperator.Multiply)
        if(_equation.A.toString == "i")
          return _equation.B
        else if(_equation.B.toString == "i")
          return _equation.A
    }
    equation
  }
  def call(equation: EquationEntity) = {
    val timer = new models.utils.Timer
    timer.start
    val resultEquation = removeKnownResults(equation)
    totalTime += timer.stop
    times += 1
    resultEquation
  }
}
