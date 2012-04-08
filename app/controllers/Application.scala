/*
 * ScalaQCMS -- Scala Quantum Circuit Model Simulator
 *
 * Copyright (c) 2012 Antti Vikman
 */

package controllers

import play.api.mvc._

import scalala.scalar.Complex
import scalala.tensor.Matrix

import models._
import utils.{Timer, QasmImport}

class DataSet {
  var circuit: String = "No circuit opened"
  var showPreProcess: Boolean = false
  val preProcessingPolicies: Set[String] = PreProcessingPolicies.getPreProcessingPolicies
  val preProcessors: Set[String] = PreProcessors.getPreProcessors
  var preProcessorStats: (EquationStatistics, EquationStatistics) = null
  var equation: String = "No equation processed"
  var showSolve: Boolean = false
  val solvingPolicies: Set[String] = SolvingPolicies.getSolvingPolicies
  val solvers: Set[String] = Solvers.getSolvers
  var solverStats: Long = 0
  var result: String = "Circuit not solved"
  var showDownload: Boolean = false
}

object Application extends Controller {

  var currentCircuit: QasmCircuit = null
  var preStat: EquationStatistics = null
  var currentEquation: EquationEntity = null
  var solvingTime: Long = 0
  var currentResult: Matrix[Complex] = null

  def index = Action {

    val dataSet = new DataSet()

    if(currentCircuit != null) {
      dataSet.circuit = currentCircuit.toString
      dataSet.showPreProcess = true
    }
    if(currentEquation != null) {
      dataSet.preProcessorStats = (preStat, currentEquation.getStatistics)
      dataSet.equation = currentEquation.toString
      dataSet.showSolve = true
      dataSet.showPreProcess = false
    }
    if(currentResult != null) {
      dataSet.solverStats = solvingTime
      dataSet.result = currentResult.toString(8,150)
      dataSet.showDownload = true
      dataSet.showSolve = false
      dataSet.showPreProcess = false
    }
    Ok(views.html.index( dataSet ))
  }

  def upload = Action(parse.multipartFormData) { request =>
    request.body.file("qasmFile").map { file =>
      //val filename = file.filename
      //val contentType = file.contentType
      currentCircuit = QasmImport.getCircuit(scala.io.Source.fromFile(file.ref.file,1024).mkString)
      currentEquation = null
      currentResult = null
      Redirect(routes.Application.index)
    }.getOrElse {
      Redirect(routes.Application.index)
    }
  }

  def preprocess = Action(parse.multipartFormData) { request =>
    if(request.body.dataParts.contains("preProcessingPolicy"))
      if(request.body.dataParts.contains("preProcessors")) {
        val preProcessingPolicy = request.body.dataParts("preProcessingPolicy")(0)
        val preProcessors = request.body.dataParts("preProcessors").toSet

        val tmpEquation = currentCircuit.toEquationEntity
        preStat = tmpEquation.getStatistics
        currentEquation = PreProcessingPolicies.process(tmpEquation,
                                                        preProcessingPolicy,
                                                        preProcessors)

        currentResult = null
      }
    Redirect(routes.Application.index)
  }

  def solve = Action(parse.multipartFormData) { request =>
    if(request.body.dataParts.contains("solvingPolicy"))
      if(request.body.dataParts.contains("solvers")) {
        val solvingPolicy = request.body.dataParts("solvingPolicy")(0)
        val solvers = request.body.dataParts("solvers").toSet
        val timer = new Timer()
        timer.start
        currentEquation = SolvingPolicies.solve(currentEquation, solvingPolicy, solvers)
        solvingTime = timer.stop
        currentResult = currentEquation.getResult
      }
    Redirect(routes.Application.index)
  }

  def download = Action {
    if(currentResult != null)
      Ok(currentResult.toString(Int.MaxValue, Int.MaxValue))
    else
      Redirect(routes.Application.index)
  }
}