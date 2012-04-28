/*
 * ScalaQCMS -- Scala Quantum Circuit Model Simulator
 *
 * Copyright (c) 2012 Antti Vikman
 */

package models

import scala.math._

import scalala.scalar.Complex
import scalala.tensor.dense.DenseMatrix

/**
 * QuantumGates is a global container of all known quantum gates
 * @todo adding and removing of gates on runtime
 */
object QuantumGates {
  val _quantumGates = Map[String, DenseMatrix[Complex]](
    "i" -> DenseMatrix((Complex(1,0),Complex(0,0)),
                       (Complex(0,0),Complex(1,0))),

    "h" -> DenseMatrix((Complex(1,0),Complex(1,0)),
                       (Complex(1,0),Complex(-1,0)))/sqrt(2.0),

    "x" -> DenseMatrix((Complex(0,0),Complex(1,0)),
                       (Complex(1,0),Complex(0,0))),

    "y" -> DenseMatrix((Complex(0,0),Complex(0,-1)),
                       (Complex(0,1),Complex(0,0))),

    "z" -> DenseMatrix((Complex(1,0),Complex(0,0)),
                       (Complex(0,0),Complex(-1,0))),

    "swap" -> DenseMatrix((Complex(1,0),Complex(0,0),Complex(0,0),Complex(0,0)),
                          (Complex(0,0),Complex(0,0),Complex(1,0),Complex(0,0)),
                          (Complex(0,0),Complex(1,0),Complex(0,0),Complex(0,0)),
                          (Complex(0,0),Complex(0,0),Complex(0,0),Complex(1,0))),

    "cnot" -> DenseMatrix((Complex(1,0),Complex(0,0),Complex(0,0),Complex(0,0)),
                          (Complex(0,0),Complex(1,0),Complex(0,0),Complex(0,0)),
                          (Complex(0,0),Complex(0,0),Complex(0,0),Complex(1,0)),
                          (Complex(0,0),Complex(0,0),Complex(1,0),Complex(0,0))),

    "toffoli" -> DenseMatrix((Complex(1,0),Complex(0,0),Complex(0,0),Complex(0,0),Complex(0,0),Complex(0,0),Complex(0,0),Complex(0,0)),
                             (Complex(0,0),Complex(1,0),Complex(0,0),Complex(0,0),Complex(0,0),Complex(0,0),Complex(0,0),Complex(0,0)),
                             (Complex(0,0),Complex(0,0),Complex(1,0),Complex(0,0),Complex(0,0),Complex(0,0),Complex(0,0),Complex(0,0)),
                             (Complex(0,0),Complex(0,0),Complex(0,0),Complex(1,0),Complex(0,0),Complex(0,0),Complex(0,0),Complex(0,0)),
                             (Complex(0,0),Complex(0,0),Complex(0,0),Complex(0,0),Complex(1,0),Complex(0,0),Complex(0,0),Complex(0,0)),
                             (Complex(0,0),Complex(0,0),Complex(0,0),Complex(0,0),Complex(0,0),Complex(1,0),Complex(0,0),Complex(0,0)),
                             (Complex(0,0),Complex(0,0),Complex(0,0),Complex(0,0),Complex(0,0),Complex(0,0),Complex(0,0),Complex(1,0)),
                             (Complex(0,0),Complex(0,0),Complex(0,0),Complex(0,0),Complex(0,0),Complex(0,0),Complex(1,0),Complex(0,0))),

    "fredkin" -> DenseMatrix((Complex(1,0),Complex(0,0),Complex(0,0),Complex(0,0),Complex(0,0),Complex(0,0),Complex(0,0),Complex(0,0)),
                             (Complex(0,0),Complex(1,0),Complex(0,0),Complex(0,0),Complex(0,0),Complex(0,0),Complex(0,0),Complex(0,0)),
                             (Complex(0,0),Complex(0,0),Complex(1,0),Complex(0,0),Complex(0,0),Complex(0,0),Complex(0,0),Complex(0,0)),
                             (Complex(0,0),Complex(0,0),Complex(0,0),Complex(1,0),Complex(0,0),Complex(0,0),Complex(0,0),Complex(0,0)),
                             (Complex(0,0),Complex(0,0),Complex(0,0),Complex(0,0),Complex(1,0),Complex(0,0),Complex(0,0),Complex(0,0)),
                             (Complex(0,0),Complex(0,0),Complex(0,0),Complex(0,0),Complex(0,0),Complex(0,0),Complex(1,0),Complex(0,0)),
                             (Complex(0,0),Complex(0,0),Complex(0,0),Complex(0,0),Complex(0,0),Complex(1,0),Complex(0,0),Complex(0,0)),
                             (Complex(0,0),Complex(0,0),Complex(0,0),Complex(0,0),Complex(0,0),Complex(0,0),Complex(0,0),Complex(1,0)))
  )

  //TODO: Refactor all of these to somewhere else
  implicit def arg(x: Complex) = math.atan2(x.imag, x.real)
  implicit def exp(x: Complex) = Complex(math.exp(x.real) * math.cos(x.imag), math.exp(x.real) * math.sin(x.imag))
  implicit def log(x: Complex) = Complex(math.log(Complex.scalar.norm(x)), arg(x))
  implicit def pow(x: Complex, y: Complex): Complex = exp(y*log(x))

  /**
   * Returns the DenseMatrix format of the requested gate
   * @param key identifier of the gate
   * @return matrix form of the gate
   */
  def get(key: String): DenseMatrix[Complex] = {
    // Quantum Fourier transform (QFT) relies on controlled phase gates that are usually named in QASM cPn
    if (key.startsWith("P")) {
      if(key.drop(1).forall(_.isDigit)) {
        val phase: Complex = (
          key.drop(1).toInt match {
            case 2 =>
              Complex(-1,0)
            case 4 =>
              Complex(0,1)
            case n: Int =>
              pow(Complex(math.E, 0), Complex(0, 2*math.Pi/n))  // e^(2*i*pi/n)
          }
          )
        return DenseMatrix((Complex(1,0),Complex(0,0)),
                           (Complex(0,0),phase))
      }
    } else if(key.startsWith("cP")) {
      if(key.drop(2).forall(_.isDigit)) {
      val phase: Complex = (
        key.drop(2).toInt match {
          case 2 =>
            Complex(-1,0)
          case 4 =>
            Complex(0,1)
          case n: Int =>
            pow(Complex(math.E, 0), Complex(0, 2*math.Pi/n))  // e^(2*i*pi/n)
        }
      )
      return DenseMatrix((Complex(1,0),Complex(0,0),Complex(0,0),Complex(0,0)),
                         (Complex(0,0),Complex(1,0),Complex(0,0),Complex(0,0)),
                         (Complex(0,0),Complex(0,0),Complex(1,0),Complex(0,0)),
                         (Complex(0,0),Complex(0,0),Complex(0,0),phase))
      }
    }
    _quantumGates.get(key).get
  }
}
