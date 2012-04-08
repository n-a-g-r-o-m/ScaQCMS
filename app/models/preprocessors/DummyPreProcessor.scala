/*
 * ScalaQCMS -- Scala Quantum Circuit Model Simulator
 *
 * Copyright (c) 2012 Antti Vikman
 */

package models.preprocessors

import models.EquationEntity

object DummyPreProcessor extends PreProcessor {
  def call(equation: EquationEntity) = {
    val timer = new models.utils.Timer
    timer.start
    val resultEquation = equation
    totalTime += timer.stop
    times += 1
    resultEquation
  }
}
