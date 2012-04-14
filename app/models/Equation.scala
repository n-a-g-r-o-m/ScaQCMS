/*
 * ScalaQCMS -- Scala Quantum Circuit Model Simulator
 *
 * Copyright (c) 2012 Antti Vikman
 */

package models

import scalala.scalar.Complex
import scalala.library.LinearAlgebra._
import scalala.tensor.Matrix


/**
 * Enumeration of mathematical operators
 */
object EquationOperator extends Enumeration {
  type EquationOperator = Value
  val Multiply, Kronecker = Value
}

/**
 * EquationStatistics is a collection of data for theoretical complexity analysis of the equation
 *
 * @param sumOperations number of arbitrary sum operations in the equation
 * @param multiplyOperations number of arbitrary multiply operations in the equation
 * @param memoryPeak peak memory requirement during solving
 * @param memoryResult memory requirement of the result
 * @param depth the depth of the equation (levels of dependencies, the higher the worse for parallelling)
 */
case class EquationStatistics(sumOperations: Long,
                              multiplyOperations: Long,
                              memoryPeak: Long,
                              memoryResult: Long,
                              depth: Int)

/**
 * EquationEntity is an abstract class that works as unified interface for Equation and EquationSymbol
 * @todo Looks like this could be refactored to Trait
 */
abstract class EquationEntity {
  /** Rows in the result matrix
   * @return  number of rows in the result matrix
   */
  def rows: Int

  /** Columns in the result matrix
   * @return number of columns in the result matrix
   */
  def cols: Int

  /** Cells in the result matrix
   * @return number of cells in the result matrix
   */
  def size: Int

  /** The result matrix
   * @return the result matrix
   */
  def getResult: Matrix[Complex]

  /** Statistics about the equation.  Especially useful for PreProcessing policies to determine
   * the effectiveness of selected pre-processing algorithm
   * @return statistics about this equation (tree)
   */
  def getStatistics: EquationStatistics

  /** Human readable presentation of the EquationEntity
   * @return  String presentation of the EquationEntity
   */
  override def toString: String
}

import EquationOperator._

/**
 * Equation is a intermediate node of "Equation-tree", that contains 2 component a (left) and b (right),
 * an op that defines the operation that shall be perfomed for a and b and result, which will be
 * defined when the equation is solved.
 * @param a left side of the equation
 * @param b right side of the equation
 * @param op arithmetic operator of the operation that shall be performed for a and b
 */
class Equation(a: EquationEntity, b: EquationEntity, op: EquationOperator) extends EquationEntity {

  if(op == EquationOperator.Multiply)
    if(a.cols != b.rows)
      throw new IllegalArgumentException("Can NOT multiply " + a.rows + "x" + a.cols + "-matrix" +
                                         " with " + b.rows + "x" + b.cols + "-matrix")
  private var _a = a
  private var _b = b
  private var _op = op
  private var _result: Matrix[Complex] = null

  def A = _a // Getter of a (left)
  def A_= (a: EquationEntity) { // Setter of a (left)
    _a = a
  }
  def B = _b
  def B_= (b: EquationEntity) {
    _b = b
  }
  def operator = _op
  def operator_= (op: EquationOperator) {
    _op = op
  }
  def result = _result
  def result_= (result: Matrix[Complex]) {
    _result = result
  }

  def rows: Int = {
    _op match {
      case Multiply =>
        _a.rows
      case Kronecker =>
        _a.rows * _b.rows
    }
  }

  def cols: Int = {
    _op match {
      case Multiply =>
        _b.cols
      case Kronecker =>
        _a.cols * _b.cols
    }
  }

  def size: Int = rows * cols
  
  def getResult: Matrix[Complex] = {
    if(result == null) {
      println("Warning: Solver hasn't set the result for " + toString)
      _op match {
        case Multiply =>
          _a.getResult * _b.getResult
        case Kronecker =>
          kron[Complex, Complex, Complex](_a.getResult, _b.getResult)
        case  _ =>
          null
      }
    }
    else
      result
  }

  def getStatistics: EquationStatistics = {
    val sumOperation = {
      _op match {
        case Multiply =>
          3L * (a.cols * size) + ((a.cols - 1) * size)
        case Kronecker =>
          3L * size /*Multiplication of Complexes contains 3 sum operations (well 2 addition and 1 subtraction */
        case  _ =>
          0
      }
    }

    val multiplyOperation = {
      _op match {
        case Multiply =>
          4L * (a.cols * size)
        case Kronecker =>
          4L * size /*Multiplication of Complexes contains 4 multiplication operations */
        case  _ =>
          0
      }
    }

    val aStat = _a.getStatistics
    val bStat = _b.getStatistics

    new EquationStatistics(
      sumOperation + aStat.sumOperations + bStat.sumOperations,
      multiplyOperation + aStat.multiplyOperations + bStat.multiplyOperations,
      16L * (size + _a.size + _b.size), /*16 is a size of Complex*/
      16L * size, /*16 is a size of Complex*/
      (aStat.depth max bStat.depth) + 1
    )
  }
  
  override def toString: String = {
    "(" + _a.toString +
    (_op match {
      case Multiply => "*"
      case Kronecker => "⊗"
      case _ => "UNKNOWN_OPERATOR"
    }) +
    _b + ")"
  }
}

/**
 * EquationSymbol is a leaf node of "Equation-tree", that has a matrix (of some quantum gate)
 * and name of that matrix
 * @param name identifier of the matrix
 * @param matrix actual matrix (if this is not defined, then matrix will be fetched from QuantumGates)
 */
class EquationSymbol(name: String,  matrix: Matrix[Complex] = null) extends EquationEntity {
  private val _matrix: Matrix[Complex] = ( if(matrix == null) QuantumGates.get(name) else matrix )

  def rows: Int = _matrix.numRows
  def cols: Int = _matrix.numCols
  def size: Int = _matrix.size
  def getResult: Matrix[Complex] = _matrix
  def getStatistics = new EquationStatistics(0, 0, 16L * size, 16L * size, 0) /*16 is a size of Complex*/
  override def toString: String = name
}