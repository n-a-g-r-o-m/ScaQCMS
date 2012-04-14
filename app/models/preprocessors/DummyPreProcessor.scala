/*
 * ScalaQCMS -- Scala Quantum Circuit Model Simulator
 *
 * Copyright (c) 2012 Antti Vikman
 */

package models.preprocessors

import models.EquationEntity

/**
 * This is a _dummy_ pre processing algorithm, which doesn't do anything for the equation.
 */
object DummyPreProcessor extends PreProcessor {

  def process(equation: EquationEntity) = {
    val timer = new models.utils.Timer
    timer.start()
    val resultEquation = equation
    totalTime += timer.stop
    times += 1
    resultEquation
  }
}
