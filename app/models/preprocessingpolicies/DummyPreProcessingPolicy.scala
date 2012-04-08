/*
 * ScalaQCMS -- Scala Quantum Circuit Model Simulator
 *
 * Copyright (c) 2012 Antti Vikman
 */

package models.preprocessingpolicies

import models.{EquationStatistics, PreProcessors, EquationEntity}


object DummyPreProcessingPolicy extends PreProcessingPolicy {
  private def improvement(previous: EquationStatistics, current: EquationStatistics) = {
    if(previous.multiplyOperations > current.multiplyOperations)
      true
    else if(previous.sumOperations > current.multiplyOperations)
      true
    else if(previous.depth > current.depth)
      true
    else
      false
  }
  
  def process(equation: EquationEntity, preProcessors: Set[String]): EquationEntity = {
    if (!preProcessors.isEmpty) {
      var previousStats: EquationStatistics = null
      var current = equation
      do {
        previousStats = current.getStatistics
        for(preProcessor <- preProcessors) {
          current = PreProcessors.process(current, preProcessor)
        }
      } while(improvement(previousStats, current.getStatistics))
      equation
    }
    PreProcessors.process(equation)
  }
}
