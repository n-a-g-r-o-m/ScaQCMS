/*
 * ScalaQCMS -- Scala Quantum Circuit Model Simulator
 *
 * Copyright (c) 2012 Antti Vikman
 */

package models

import preprocessingpolicies.{PreProcessingPolicy, DummyPreProcessingPolicy}

/**
 * PreProcessingPolicies is a global container of all known pre processing policies
 * @todo adding and removing of policies on runtime
 */
object  PreProcessingPolicies {
  private val _preProcessingPolicies = Map[String, PreProcessingPolicy] (
    "dummy" -> DummyPreProcessingPolicy
  )

  def getPreProcessingPolicies = _preProcessingPolicies.keySet

  def process(equation: EquationEntity,
            preProcessingPolicy: String = "dummy",
            preProcessors: Set[String] = PreProcessors.getPreProcessors): EquationEntity = {
    if(equation == null)
      throw new NullPointerException("Variable equation can not be null")
    if(!_preProcessingPolicies.contains(preProcessingPolicy))
      throw new MatchError("No such preProcessing policy as \"" + preProcessingPolicy + "\"")
    _preProcessingPolicies.get(preProcessingPolicy).get.process(equation, preProcessors)
  }
}

