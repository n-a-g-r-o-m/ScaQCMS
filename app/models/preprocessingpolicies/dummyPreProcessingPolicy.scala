/*
 * ScalaQCMS -- Scala Quantum Circuit Model Simulator
 *
 * Copyright (c) 2012 Antti Vikman
 */

package models.preprocessingpolicies

import models.{PreProcessors, EquationEntity}

object dummyPreProcessingPolicy extends PreProcessingPolicy {
  def process(equation: EquationEntity, preProcessors: Set[String]): EquationEntity = {
    if (preProcessors.isEmpty)
      PreProcessors.process(equation)
    else
      PreProcessors.process(equation, preProcessors.first)
  }
}
