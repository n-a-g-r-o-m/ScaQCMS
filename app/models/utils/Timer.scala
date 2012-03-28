/*
 * ScalaQCMS -- Scala Quantum Circuit Model Simulator
 *
 * Copyright (c) 2012 Antti Vikman
 */

package models.utils

class Timer {
  private var startTime: Long = 0L
  private var stopTime: Long = 0L

  def start {
    startTime = System.currentTimeMillis
  }

  def stop = {
    stopTime = System.currentTimeMillis
    (stopTime - startTime) * 1000
  }
}
