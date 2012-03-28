/*
 * ScalaQCMS -- Scala Quantum Circuit Model Simulator
 *
 * Copyright (c) 2012 Antti Vikman
 */

package models

import preprocessors.{PreProcessor, dummyPreProcessor}

object PreProcessors {
  
  val _preProcessors = Map[String, PreProcessor](
    "dummy" -> dummyPreProcessor
  ) 
  
  def getPreProcessors = _preProcessors.keySet

  def process(equation: EquationEntity, preProcessor: String = "dummy"): EquationEntity = {
    if(equation == null)
      throw new NullPointerException("Variable equation can not be null")
    if(!_preProcessors.contains(preProcessor))
      throw new MatchError("No such preprocessor as \"" + preProcessor + "\"")

    _preProcessors.get(preProcessor).get.call(equation)
  }
}
