/*
 * ScalaQCMS -- Scala Quantum Circuit Model Simulator
 *
 * Copyright (c) 2012 Antti Vikman
 */

package models

import scala.math._

import scalala.scalar.Complex
import scalala.tensor.dense.DenseMatrix

object QuantumGates {
  val _quantumGates = Map[String, DenseMatrix[Complex]](
    "i" -> DenseMatrix((Complex(1,0),Complex(0,0)),
                       (Complex(0,0),Complex(1,0))),

    "h" -> DenseMatrix((Complex(1,0),Complex(1,0)),
                       (Complex(1,0),Complex(-1,0)))/sqrt(2.0),

    "x" -> DenseMatrix((Complex(0,0),Complex(1,0)),
                       (Complex(1,0),Complex(0,0))),

    "swap" -> DenseMatrix((Complex(1,0),Complex(0,0),Complex(0,0),Complex(0,0)),
                          (Complex(0,0),Complex(0,0),Complex(1,0),Complex(0,0)),
                          (Complex(0,0),Complex(1,0),Complex(0,0),Complex(0,0)),
                          (Complex(0,0),Complex(0,0),Complex(0,0),Complex(1,0))),

    "cP2" -> DenseMatrix((Complex(1,0),Complex(0,0),Complex(0,0),Complex(0,0)),
                         (Complex(0,0),Complex(1,0),Complex(0,0),Complex(0,0)),
                         (Complex(0,0),Complex(0,0),Complex(1,0),Complex(0,0)),
                         (Complex(0,0),Complex(0,0),Complex(0,0),Complex(1,0))),

    "cP4" -> DenseMatrix((Complex(1,0),Complex(0,0),Complex(0,0),Complex(0,0)),
                         (Complex(0,0),Complex(1,0),Complex(0,0),Complex(0,0)),
                         (Complex(0,0),Complex(0,0),Complex(1,0),Complex(0,0)),
                         (Complex(0,0),Complex(0,0),Complex(0,0),Complex(1,0))),

    "cP8" -> DenseMatrix((Complex(1,0),Complex(0,0),Complex(0,0),Complex(0,0)),
                         (Complex(0,0),Complex(1,0),Complex(0,0),Complex(0,0)),
                         (Complex(0,0),Complex(0,0),Complex(1,0),Complex(0,0)),
                         (Complex(0,0),Complex(0,0),Complex(0,0),Complex(1,0)))
  )

  def get(key: String): DenseMatrix[Complex] = _quantumGates.get(key).get
}
