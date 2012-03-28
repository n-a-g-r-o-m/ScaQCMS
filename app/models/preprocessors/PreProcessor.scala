/*
 * ScalaQCMS -- Scala Quantum Circuit Model Simulator
 *
 * Copyright (c) 2012 Antti Vikman
 */

package models.preprocessors

import models.EquationEntity

case class PreProcessorStatistics(time: Long)

abstract class PreProcessor {
  //TODO: I Guess trait would be more correct
  var totalTime, times = 0L

  def call(equation: EquationEntity): EquationEntity

  def statistics: PreProcessorStatistics = new PreProcessorStatistics(totalTime / times)
}
