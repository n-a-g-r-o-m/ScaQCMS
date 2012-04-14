/*
 * ScalaQCMS -- Scala Quantum Circuit Model Simulator
 *
 * Copyright (c) 2012 Antti Vikman
 */

package models.utils

import models.{QasmCircuit,QasmCommand}
import collection.SortedMap

/**
 * QasmImport is a util for converting data from QASM format to QasmCircuit.
 * NOTE: Current version of ScaQCMS can only handle circuits that satisfy
 * the linear nearest neighbour (LNN) architecture.
 * @todo Current implementation uses SortedMap, which forces qubits used in
 *       QASM file to be in alphabetical order. Some form of Indexed map
 *       should be used to remove this requirement from the QASM
 */
object QasmImport {

  /**
   * finds the length of the longest QasmCommand vector stored in the map
   * @param map the map to be evaluated
   * @return length of the longest Vector stored in the map
   */
  private def getLongest(map: SortedMap[String, Vector[QasmCommand]]): Int = {
    map.foldLeft(0)((longest, mapEntry) => longest max mapEntry._2.size)
  }

  /**
   * converts a single QASM format line to QasmCommand
   * @param line line to be converted
   * @return converted QasmCommand
   */
  private def stringToQasmCommand(line: String) = {
    line.split("[ \\t]", 2) match {
      case Array(name, qubits) => new QasmCommand(name.trim, qubits.trim.split(","))
      case _ => throw new Exception("Invalid qasm line: \"" + line + "\"")
    }
  }

  /**
   * converts QASM format string to a map of qubits and QASM commands related to those qubits
   * @param qasm QASM format data to be converted
   * @return map of qubits and QASM commands related to those qubits
   */
  private def qasm2commandMap(qasm: String): SortedMap[String, Vector[QasmCommand]] = {
    var commandMap = SortedMap.empty[String, Vector[QasmCommand]]

    for (line <- qasm.split("\\n"); //For each line in qasm,
         trimmedLine = line.trim; //
         if trimmedLine.length > 0; //that is not empty
         if !trimmedLine.startsWith("#")) {
      //or a comment line

      val qasmCommand = stringToQasmCommand(trimmedLine)

      qasmCommand.name match {
        case "def" | "defbox" => //Special command to introduce new gate
          /*This only introduces graphical label, so not much of a use for us.
            We could request user to input corresponding matrix, when we encounter this
            but that would require building of some sort of event bus, so it might
            be simpler to ignore these and request the matrix if the QasmCircuit
            returned to controller contains unknown gates.
           */
        case "qubit" => //Special "gate" to introduce a new qubit
          for (qubit <- qasmCommand.qubits)
            commandMap += ((qubit, Vector.empty[QasmCommand])) //Add a qubit to command map
        case _ =>
          for (qubit <- qasmCommand.qubits) {
            commandMap += ((qubit, commandMap(qubit) :+ qasmCommand)) //Add command (gate) to qubits effected
          }
      }
    }

    var i = 0
    //We need to use while, to guarantee that getLongest is called on every round
    while (i < getLongest(commandMap)) {
      for ((qubit, gates) <- commandMap) {
        if (gates.size <= i)
          commandMap += ((qubit, gates :+ new QasmCommand("i", Array(qubit))))
        else {
          val command = gates(i)
          if (command.qubits.size > 1) {
            var ok = true
            for (qubit <- command.qubits) {
              val pairCommand = commandMap(qubit)(i)
              if (command != pairCommand)
                ok = false
            }
            if (!ok) {
              val newGates: Vector[QasmCommand] = {
                //We need to do it like this, since Scala Vector doesn't have Insert
                (gates.take(i) :+ new QasmCommand("i", Array(qubit))) ++ gates.drop(i)
              }
              commandMap += ((qubit, newGates))
            }
          }
        }
      }
      i += 1
    }
    commandMap
  }

  /**
   * Converts string containing data in QASM format to QasmCircuit
   * @param gasm QASM format string to be converted
   * @return converted QasmCircuit
   */
  def getCircuit(gasm: String): QasmCircuit = {
    new QasmCircuit(qasm2commandMap(gasm))
  }
}
