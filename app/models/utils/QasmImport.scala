/*
 * ScalaQCMS -- Scala Quantum Circuit Model Simulator
 *
 * Copyright (c) 2012 Antti Vikman
 */

package models.utils

import models.{QasmCircuit,QasmCommand}

object QasmImport {

  private def getLongest(map: Map[String, Vector[QasmCommand]]): Int = {
    map.foldLeft(0)((longest, mapEntry) => longest max mapEntry._2.size)
  }

  private def stringToQasmCommand(line: String) = {
    line.split("[ \\t]", 2) match {
      case Array(name, qubits) => new QasmCommand(name.trim, qubits.trim.split(","))
      case _ => throw new Exception("Invalid qasm line: \"" + line + "\"")
    }
  }

  private def qasm2commandMap(qasm: String): Map[String, Vector[QasmCommand]] = {
    var commandMap = Map.empty[String, Vector[QasmCommand]]

    for (line: String <- qasm.split("\\n"); //For each line in qasm,
         trimmedLine = line.trim; //
         if trimmedLine.length > 0; //that is not empty
         if !trimmedLine.startsWith("#")) {
      //or a comment line

      val qasmCommand = stringToQasmCommand(trimmedLine)

      qasmCommand.name match {
        case "def" | "defbox" => //Special command to introduce new gate
        //This only introduces graphical label, so not much of a use for us
        case "qubit" => //Special gate to introduce a new qubit
          for (qubit <- qasmCommand.qubits)
            commandMap += ((qubit, Vector.empty[QasmCommand])) //Add a qubit to command map
        case _ =>
          for (qubit <- qasmCommand.qubits) {
            commandMap += ((qubit, commandMap(qubit) :+ qasmCommand)) //Add command (gate) to qubits effected
          }
      }
    }

    var i = 0
    while (i < getLongest(commandMap)) {
      //We need to use while, to guarantee that getLongest is called on every round
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

  def getCircuit(gasm: String): QasmCircuit = {
    new QasmCircuit(qasm2commandMap(gasm))
  }
}
