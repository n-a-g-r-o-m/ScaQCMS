/*
 * ScalaQCMS -- Scala Quantum Circuit Model Simulator
 *
 * Copyright (c) 2012 Antti Vikman
 */

package models.preprocessingpolicies

import models.{EquationStatistics, PreProcessors, EquationEntity}

/**
 * This is a _dummy_ preprocessing policy  that simply tries to use all of the given PreProcessors
 * until they don't give any benefit anymore. Some decent Team Algorithm implementation will outperform
 * this 100:1, that is why we call this _dummy_.
  */
object DummyPreProcessingPolicy extends PreProcessingPolicy {
  /** Simple private function to determine was there any improvements from previous state.
   *
   * @param previous statistics
   * @param current statistics
   * @return true if we have improved or false if we haven't
   */
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
    if (!preProcessors.isEmpty) { // If we have PreProcessors, we will use them
      var previousStats: EquationStatistics = null
      var current = equation
      do {
        previousStats = current.getStatistics
        for(preProcessor <- preProcessors) {                      //Lets try to use all the preProcessors we got
          current = PreProcessors.process(current, preProcessor)
        }
      } while(improvement(previousStats, current.getStatistics))  //and do so as long as they do some good
      equation
    } else // If we don't have PreProcessors, then we use the default (which is dummy, which doesn't do anything)
      PreProcessors.process(equation)
  }
}
