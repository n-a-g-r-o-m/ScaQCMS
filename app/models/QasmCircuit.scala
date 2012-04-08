/*
 * ScalaQCMS -- Scala Quantum Circuit Model Simulator
 *
 * Copyright (c) 2012 Antti Vikman
 */

package models

import collection.SortedMap

case class QasmCommand(name: String, qubits: Array[String])

class QasmCircuit(commandMap: SortedMap[String, Vector[QasmCommand]]) {

  private def getLongest(map: SortedMap[String, Vector[QasmCommand]]): Int = {
    map.foldLeft(0)((longest, mapEntry) => longest max mapEntry._2.size)
  }

  override def toString: String = {
    var returnString = ""
    for ((qubit, gates) <- commandMap) {
     // We go trough each qubit
     returnString += (qubit + ":\t") // and start a line with qubit's name,
     for (gate <- gates) {
       // then we go through each gate the qubit has
       returnString += (gate.name + "\t") // and add them to the line.
     }
     returnString += "\n" // Before going to next qubit, we add new line
    }
    returnString
  }

  def toEquationEntity: EquationEntity = {
    var result: EquationEntity = null
    var columnResult: EquationEntity = null

    for (i <- 0 until getLongest(commandMap)) {
     // We go trough all the columns
     var j = 0 // reading a column from up
     while (j < commandMap.size) {
       // to down.
       val gate = commandMap.toList(j)._2(i)
       val name = gate.name
       if (columnResult == null) {
         // If we are on the first row
         columnResult = new EquationSymbol(name) // then we just add the gate,
       } else {
         // else
         columnResult = new Equation(// we will make equation
           columnResult, // from previous equation/gate
           new EquationSymbol(name), // and current gate
           EquationOperator.Kronecker) // by tensoring them together.
       }
       j += gate.qubits.size //Then we jump to next unhandled gate (Note: if 2 qubit gate we can skip next etc.)
     }
     if (result == null) // If it was the first column
       result = columnResult // then we just add
     else // else
       result = new Equation(// we will make equation (NOTE: "reversed" order)
         columnResult, // from the current column
         result, // and previous column(s)
         EquationOperator.Multiply) // by multiplying them together
     columnResult = null
    }
    result
  }
}
