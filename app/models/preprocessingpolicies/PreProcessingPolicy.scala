/*
 * ScalaQCMS -- Scala Quantum Circuit Model Simulator
 *
 * Copyright (c) 2012 Antti Vikman
 */

package models.preprocessingpolicies

import models.EquationEntity

abstract class PreProcessingPolicy {
  def process(equation: EquationEntity, preProcessors: Set[String]): EquationEntity
}
