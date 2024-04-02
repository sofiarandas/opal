/* BSD 2-Clause License - see OPAL/LICENSE for details. */
package org.opalj
package tac

import org.opalj.br.Method
import org.opalj.br.analyses.Project
import org.opalj.br.instructions.{DRETURN, FRETURN, IRETURN, Instruction, LRETURN}
import org.opalj.value.ValueInformation

import java.io.File
import scala.collection.mutable.ArrayBuffer

object TACtoBC {

  /**
   * Compiles the Three-Address Code (TAC) representation for all methods in the given .class file.
   *
   * @param file A File object representing the .class file to be analyzed and compiled into TAC.
   * @return A Map associating each method in the class file with its corresponding TAC representation.
   */
  def compileTAC(file: File): Map[Method, AITACode[TACMethodParameter, ValueInformation]] = {
    val p = Project(file)
    val tacProvider = p.get(LazyDetachedTACAIKey)

    // Store the TAC results in a map
    val methodTACMap = scala.collection.mutable.Map.empty[Method, AITACode[TACMethodParameter, ValueInformation]]

    for {
      cf <- p.allProjectClassFiles
      m <- cf.methods
      if m.body.isDefined
    } {
      val tac = tacProvider(m)
      methodTACMap += (m -> tac)
    }

    methodTACMap.toMap
  }

  /**
   * Translates the TAC representations of methods back to bytecode, encapsulated within OPAL's Code structure.
   *
   * This method iterates over each method's TAC representation and generates a corresponding sequence of
   * bytecode instructions, effectively reversing the process of TAC generation. The resulting bytecode
   * is suitable for execution by the JVM.
   *
   * @param tacs A Map containing the TAC representations of methods to be translated back to bytecode.
   * @return A Map associating each method with its newly generated bytecode, wrapped in OPAL's Code structure.
   */
  def translateTACtoBC(tacs: Map[Method, AITACode[TACMethodParameter, ValueInformation]]): Map[Method, Array[Instruction]] = {
    tacs.map { case (method, tacCode) =>
      // Convert the TAC representation back to bytecode for each method
      val bytecodeInstructions = translateSingleTACtoBC(tacCode)
      method -> bytecodeInstructions
    }
  }

  /**
   * Converts the TAC representation of a single method into bytecode instructions.
   *
   * This helper method processes one method's TAC representation at a time, converting it into a sequence
   * of bytecode instructions. It handles various types of TAC statements and expressions, translating them
   * into their equivalent bytecode form.
   *
   * @param tac The TAC representation of a method to be converted into bytecode.
   * @return An array of bytecode instructions representing the method's functionality, ready to be executed by the JVM.
   */
  def translateSingleTACtoBC(tac: AITACode[TACMethodParameter, ValueInformation]): Array[Instruction] = {
    val byteCodeInstructions = ArrayBuffer[Instruction]()

    tac.stmts.foreach {
      case ReturnValue(pc, expr) =>
        expr match {
          case _: IntConst => byteCodeInstructions += IRETURN
          case _: LongConst => byteCodeInstructions += LRETURN
          case _: FloatConst => byteCodeInstructions += FRETURN
          case _: DoubleConst => byteCodeInstructions += DRETURN
          //ToDo: case for ARETURN
        }
      // LOAD instructions
      /*case Assignment(_, SimpleVar(index, _), expr) => // This assumes `SimpleVar` is the correct way to match variables in your context
        expr match {
          case _: 0 | 1 => byteCodeInstructions += BALOAD // Potentially a boolean value
          //case _: CharConst =>
          case _: IntConst => byteCodeInstructions += ILOAD(index) // Assuming `ILOAD(index)` is a correct representation for loading an integer from local variable `index`
          case _: LongConst =>
        }*/
      case  If(pc, left, condition, right, target) => println("it was an if")
      case _ =>
    }

    byteCodeInstructions.toArray
  }

  def main(args: Array[String]): Unit = {
    if(args.length != 1) {
      println("Usage: TACtoBC <path to class or jar file>")
      return
    }

    val file = new File(args(0))
    if(!file.exists()) {
      println(s"File ${file.getPath} does not exist.")
      return
    }

    val tacs = compileTAC(file)

    // Print out TAC
    tacs.foreach { case (method, tac) =>
      tac.detach()
      println(s"Method: ${method.toJava}")
      println(tac.toString)
    }

    // Print out the translation from TAC to Bytecode
   val byteCodes = translateTACtoBC(tacs)
    byteCodes.foreach { case (method, bytecode) =>
      println(s"Method: ${method.toJava}")
      println(bytecode.toString)
    }
  }
}
