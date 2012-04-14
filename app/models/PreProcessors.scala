/*
 * ScalaQCMS -- Scala Quantum Circuit Model Simulator
 *
 * Copyright (c) 2012 Antti Vikman
 */

package models

import preprocessors._

/**
 * PreProcessors is a global container of all known pre processors
 * @todo adding and removing of pre processors on runtime
 */
object PreProcessors {
  
  val _preProcessors = Map[String, PreProcessor](
    "dummy" -> DummyPreProcessor,
    "balance" -> BalancePreProcessor,
    "operator swap" -> OperatorSwapPreProcessor,
    "known results" -> KnownResultsPreProcessor
  ) 
  
  def getPreProcessors = _preProcessors.keySet

  def process(equation: EquationEntity, preProcessor: String = "dummy"): EquationEntity = {
    if(equation == null)
      throw new NullPointerException("Variable equation can not be null")
    if(!_preProcessors.contains(preProcessor))
      throw new MatchError("No such preprocessor as \"" + preProcessor + "\"")

    _preProcessors.get(preProcessor).get.process(equation)
  }
}
