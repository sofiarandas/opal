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
   *
   * @param file coompiles the TAC representation of the given .class file
   * @return a map containing all TAC stmts
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

  def translateTACtoBC(tacs: Map[Method, AITACode[TACMethodParameter, ValueInformation]]): Map[Method, Array[Instruction]] = {
    tacs.map { case (method, tacCode) =>
      // Convert the TAC representation back to bytecode for each method
      val bytecodeInstructions = translateSingleTACtoBC(tacCode)
      method -> bytecodeInstructions
    }
  }

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
