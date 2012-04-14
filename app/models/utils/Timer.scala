/*
 * ScalaQCMS -- Scala Quantum Circuit Model Simulator
 *
 * Copyright (c) 2012 Antti Vikman
 */

package models.utils

/**
 * Timer is a simple util to measure a time
 */
class Timer {
  private var startTime: Long = 0L
  private var stopTime: Long = 0L

  /**
   * start() will simply start the timer
   */
  def start() {
    startTime = System.currentTimeMillis
  }

  /**
   * stop will return the time passed since the Timer was started.
   * Calling stop doesn't really stop the timer, so you can call
   * stop multiple times to get interval times.
   * @return time in microseconds that passed since start() was called.
   */
  def stop = {
    stopTime = System.currentTimeMillis
    (stopTime - startTime) * 1000
  }
}
