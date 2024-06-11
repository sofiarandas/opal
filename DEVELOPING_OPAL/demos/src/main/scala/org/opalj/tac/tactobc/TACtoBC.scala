/* BSD 2-Clause License - see OPAL/LICENSE for details. */
package org.opalj.tac.tactobc

import org.opalj.br.Method
import org.opalj.br.analyses.Project
import org.opalj.br.instructions.{GOTO, IF_ICMPEQ, IF_ICMPGE, IF_ICMPGT, IF_ICMPLE, IF_ICMPLT, IF_ICMPNE, Instruction, LOOKUPSWITCH, TABLESWITCH}
import org.opalj.collection.immutable.IntIntPair
import org.opalj.tac._
import org.opalj.value.ValueInformation

import java.io.File
import scala.collection.immutable.ArraySeq
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
   * Compiles and prints the bytecode representation for all methods in the given .class file.
   *
   * @param file The .class file or JAR archive to be analyzed.
   * @return A Map associating each method in the class file with its bytecode instructions.
   */
  def compileByteCode(file: File): Map[Method, Array[String]] = {
    val p = Project(file)

    // A map to store the bytecode representation of each method
    val methodByteCodeMap = scala.collection.mutable.Map.empty[Method, Array[String]]

    for {
      cf <- p.allProjectClassFiles
      method <- cf.methods
      if method.body.isDefined
    } {
      // Convert the body's instructions to a human-readable format
      val instructions = method.body.get.instructions.zipWithIndex.map { case (instr, index) =>
        s"$index: ${instr}"
      }
      methodByteCodeMap += (method -> instructions.toArray)

      // Print the bytecode for each method
      println(s"Method: ${method.toJava}")
      instructions.foreach(println)
    }

    methodByteCodeMap.toMap
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
  def translateTACtoBC(tacs: Map[Method, AITACode[TACMethodParameter, ValueInformation]]): Map[Method, ArrayBuffer[(Int, Instruction)]] = {
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
  def translateSingleTACtoBC(tac: AITACode[TACMethodParameter, ValueInformation]): ArrayBuffer[(Int, Instruction)] = {
    val generatedByteCodeWithPC = ArrayBuffer[(Int, Instruction)]()
    var currentPC = 0
    val tacTargetToByteCodePcs = ArrayBuffer[(Int, Int)]()
    val switchCases = ArrayBuffer[(Int, Int)]() // To store switch case targets
    //first pass
    val tacStmts = tac.stmts.zipWithIndex
    tacStmts.foreach { case (stmt, _) =>
      stmt match {
        case Assignment(_, targetVar, expr) =>
          tacTargetToByteCodePcs += ((-1, currentPC))
          currentPC = StmtProcessor.processAssignment(targetVar, expr, generatedByteCodeWithPC, currentPC)
        case ExprStmt(_, expr) =>
          tacTargetToByteCodePcs += ((-1, currentPC))
          currentPC = ExprUtils.processExpression(expr, generatedByteCodeWithPC, currentPC)
        case If(_, left, condition, right, target) =>
          tacTargetToByteCodePcs += ((target, currentPC))
          currentPC = StmtProcessor.processIf(left, condition, right, target, generatedByteCodeWithPC, currentPC)
        case Goto(_, target) =>
          tacTargetToByteCodePcs += ((target, currentPC))
          currentPC = StmtProcessor.processGoto(generatedByteCodeWithPC, currentPC)
        case Switch(_, defaultTarget, index, npairs) =>
          npairs.foreach { pair =>
            switchCases += ((pair._1, pair._2))//case values to jump target
          }
          tacTargetToByteCodePcs += ((defaultTarget, currentPC))
          currentPC = StmtProcessor.processSwitch(defaultTarget, index, npairs, generatedByteCodeWithPC, currentPC)
        case VirtualMethodCall(_, declaringClass, isInterface, name, descriptor, receiver, params) =>
          tacTargetToByteCodePcs += ((-1, currentPC))
          currentPC = StmtProcessor.processVirtualMethodCall(declaringClass, isInterface, name, descriptor, receiver, params, generatedByteCodeWithPC, currentPC)
        case ReturnValue(_, expr) =>
          tacTargetToByteCodePcs += ((-1, currentPC))
          currentPC = StmtProcessor.processReturnValue(expr, generatedByteCodeWithPC, currentPC)
        case Return(_) =>
          tacTargetToByteCodePcs += ((-1, currentPC))
          currentPC = StmtProcessor.processReturn(generatedByteCodeWithPC, currentPC)
        case _ =>
      }
    }
    //second pass -> this time through the translated bytecode to calculate the right branching targets
    val result = ArrayBuffer[(Int, Instruction)]()
    // Index for TAC statements
    var tacTargetToByteCodePcsIndex = 0

    generatedByteCodeWithPC.zipWithIndex.foreach {
      case ((pc, instruction), _) =>
        // Match and update branch instructions
        val updatedInstruction = instruction match {
          case IF_ICMPEQ(-1) =>
            tacTargetToByteCodePcsIndex -= 1
            val instruction = IF_ICMPEQ(updateBranchTargets(tacTargetToByteCodePcs, tacTargetToByteCodePcsIndex))
            tacTargetToByteCodePcsIndex += 1
            instruction
          case IF_ICMPNE(-1) =>
            tacTargetToByteCodePcsIndex -= 1
            val instruction = IF_ICMPNE(updateBranchTargets(tacTargetToByteCodePcs, tacTargetToByteCodePcsIndex))
            tacTargetToByteCodePcsIndex += 1
            instruction
          case IF_ICMPLT(-1) =>
            tacTargetToByteCodePcsIndex -= 1
            val instruction = IF_ICMPLT(updateBranchTargets(tacTargetToByteCodePcs, tacTargetToByteCodePcsIndex))
            tacTargetToByteCodePcsIndex += 1
            instruction
          case IF_ICMPLE(-1) =>
            tacTargetToByteCodePcsIndex -= 1
            val instruction = IF_ICMPLE(updateBranchTargets(tacTargetToByteCodePcs, tacTargetToByteCodePcsIndex))
            tacTargetToByteCodePcsIndex += 1
            instruction
          case IF_ICMPGT(-1) =>
            tacTargetToByteCodePcsIndex -= 1
            val instruction = IF_ICMPGT(updateBranchTargets(tacTargetToByteCodePcs, tacTargetToByteCodePcsIndex))
            tacTargetToByteCodePcsIndex += 1
            instruction
          case IF_ICMPGE(-1) =>
            tacTargetToByteCodePcsIndex -= 1
            val instruction = IF_ICMPGE(updateBranchTargets(tacTargetToByteCodePcs, tacTargetToByteCodePcsIndex))
            tacTargetToByteCodePcsIndex += 1
            instruction
          case GOTO(-1) =>
            GOTO(updateBranchTargets(tacTargetToByteCodePcs, tacTargetToByteCodePcsIndex))
          case LOOKUPSWITCH(defaultOffset, matchOffsets) =>
            tacTargetToByteCodePcsIndex -= 1
            val updatedMatchOffsets = matchOffsets.map { case IntIntPair(caseValue, _) =>
              val tacTarget = findTacTarget(switchCases, caseValue)
              IntIntPair(caseValue, updateSwitchTargets(tacTargetToByteCodePcs, tacTarget))
            }
            val updatedDefaultOffset = updateSwitchTargets(tacTargetToByteCodePcs, defaultOffset)
            tacTargetToByteCodePcsIndex += 1
            LOOKUPSWITCH(updatedDefaultOffset, updatedMatchOffsets)
          case TABLESWITCH(defaultOffset, low, high, jumpOffsets) =>
            val updatedJumpOffsets = jumpOffsets.zipWithIndex.map { case (_, index) =>
              updateBranchTargets(switchCases, low + index)
            }
            TABLESWITCH(defaultOffset, low, high, updatedJumpOffsets.to(ArraySeq))
          case _ =>
            instruction
        }
        result += ((pc, updatedInstruction))

        // Only increment tacIndex when the current instruction corresponds to a TAC statement
        if (tacTargetToByteCodePcsIndex < tacStmts.length && directAssociationExists(tacTargetToByteCodePcs, tacTargetToByteCodePcs(tacTargetToByteCodePcsIndex)._1, pc)) {
          tacTargetToByteCodePcsIndex += 1
        }
    }
    result
  }

  def findTacTarget(npairs: ArrayBuffer[(Int, Int)], caseValue: Int): Int = {
    val tacTarget = npairs.find(_._1 == caseValue).map(_._2).get
    tacTarget
  }

  def updateBranchTargets(tacTargetToByteCodePcs: ArrayBuffer[(Int, Int)], tacTargetToByteCodePcsIndex: Int): Int = {
    val tacTarget = tacTargetToByteCodePcs(tacTargetToByteCodePcsIndex)._1
    val byteCodeTarget = tacTargetToByteCodePcs(tacTarget)._2
    byteCodeTarget
  }

  def updateSwitchTargets(tacTargetToByteCodePcs: ArrayBuffer[(Int, Int)], tacTarget: Int): Int = {
    val byteCodeTarget = tacTargetToByteCodePcs(tacTarget)._2
    byteCodeTarget
  }

  def directAssociationExists(tacTargetToByteCodePcs: ArrayBuffer[(Int, Int)], tacTarget: Int, bytecodePC: Int): Boolean = {
    tacTargetToByteCodePcs.exists { case (tacGoto, bcPC) => (tacGoto, bcPC) == (tacTarget, bytecodePC) }
  }

  def main(args: Array[String]): Unit = {
    if (args.length != 1) {
      println("Usage: TACtoBC <path to class or jar file>")
      return
    }

    val file = new File(args(0))
    if (!file.exists()) {
      println(s"File ${file.getPath} does not exist.")
      return
    }

    compileByteCode(file)

    val tacs = compileTAC(file)

    // Print out TAC
    tacs.foreach { case (method, tac) =>
      tac.detach()
      println(s"Method: ${method.toJava}")
      println(tac.toString)
      println("\n")

    }

    // Print out the translation from TAC to Bytecode
    val byteCodes = translateTACtoBC(tacs)
    byteCodes.foreach { case (method, bytecode) =>
      println(s"Method: ${method.toJava}")
      bytecode.foreach(instr => println(instr.toString))
    }

  }
}
