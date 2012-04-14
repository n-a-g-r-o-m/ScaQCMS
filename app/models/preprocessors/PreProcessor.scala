/*
 * ScalaQCMS -- Scala Quantum Circuit Model Simulator
 *
 * Copyright (c) 2012 Antti Vikman
 */

package models.preprocessors

import models.EquationEntity

/**
 * Container of execution statistics of pre processor.
 * @param time is just and example that could be here
 */
case class PreProcessorStatistics(time: Long)

/**
 * PreProcessor is an abstract class to provide unified interface for all the pre processors
 */
abstract class PreProcessor {
  var totalTime, times = 0L

  /**
   * Pre processes the given equation
   * @param equation to be pre processed
   * @return pre processed equation
   */
  def process(equation: EquationEntity): EquationEntity

  /**
   * Returns the statistics of this PreProcessor. This data can be consumed by the PreProcessingPolicy
   * when choosing suitable pre processor(s).
   * @return PreProcessorStatistics that describes the execution statistics of previous executions
   */
  def statistics: PreProcessorStatistics = new PreProcessorStatistics(totalTime / times)
}
