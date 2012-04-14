/*
 * ScalaQCMS -- Scala Quantum Circuit Model Simulator
 *
 * Copyright (c) 2012 Antti Vikman
 */

package models.preprocessingpolicies

import models.EquationEntity

/**
 * PreProcessingPolicy is an abstract class to provide unified interface for all pre processing policies.
 * @todo Looks like this could be refactored to Trait
 */
abstract class PreProcessingPolicy {
  /**
   * Pre processes the equation using the given set of pre processors
   *
   * @param equation is the equation to be pre processed
   * @param preProcessors is a set of PreProcessor names that can be used in pre processing
   * @return the pre processed equation
   */
  def process(equation: EquationEntity, preProcessors: Set[String]): EquationEntity
}
