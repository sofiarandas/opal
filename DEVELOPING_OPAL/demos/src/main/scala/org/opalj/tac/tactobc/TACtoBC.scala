/* BSD 2-Clause License - see OPAL/LICENSE for details. */
package org.opalj.tac.tactobc

//import org.opalj.ba.toDA
//import org.opalj.bc.Assembler
import org.opalj.ba.toDA
import org.opalj.bc.Assembler
import org.opalj.br.{Code, Method, ObjectType}
import org.opalj.br.analyses.Project
import org.opalj.br.instructions.{GOTO, IF_ICMPEQ, IF_ICMPGE, IF_ICMPGT, IF_ICMPLE, IF_ICMPLT, IF_ICMPNE, Instruction, LOOKUPSWITCH, TABLESWITCH}
import org.opalj.br.reader.Java8Framework
import org.opalj.collection.immutable.IntIntPair
import org.opalj.da.ClassFileReader.ClassFile
import org.opalj.io.writeAndOpen
import org.opalj.util.InMemoryClassLoader

import java.io.ByteArrayInputStream
import java.nio.file.{Files, Paths}
//import org.opalj.da.ClassFileReader.ClassFile
//import org.opalj.io.writeAndOpen
//import org.opalj.util.InMemoryClassLoader

//import java.io.ByteArrayInputStream
//import java.nio.file.Paths

//import org.opalj.da.ClassFileReader.ClassFile
//import org.opalj.io.writeAndOpen
import org.opalj.tac._
import org.opalj.value.ValueInformation

import java.io.File
import scala.collection.immutable.ArraySeq
import scala.collection.mutable.ArrayBuffer

object TACtoBC {

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

    //todo: fill array of instructions with nulls
    /*val byteCodesWithNulls = byteCodes.foreach{ case (method, bytecode) =>
      val bytecodeArrayWithNulls = new Array[Instruction](bytecode.last._1)
      val index = 0
      while(index < bytecodeArrayWithNulls.length) {
        val instruction = byteCodes.get(method).get
        if(index.==(b)){

        }
      }
    }*/

   val TheType = ObjectType("org/opalj/ba/testingTAC/HelloWorldToString")

    val in = () => this.getClass.getResourceAsStream("/org/opalj/ba/testingTAC/HelloWorldToString.class")
    val cf = Java8Framework.ClassFile(in).head
    val newMethods = {
      for (m <- cf.methods) yield {
        m.body match {
          case None =>
            m.copy() // methods which are native and abstract ...
          case Some(originalBody) =>
            //Using find because of the extra methods that do contain the name of the method but are not part of the original file
            byteCodes.find(bc => bc._1.name.contains(m.name)) match {
              case Some((_, instructions)) =>
                // Prepare new instructions array with null values where necessary
                val maxPc = instructions.map(_._1).max
                val newInstructionsWithNulls = new Array[Instruction](maxPc + 1)

                // Initialize array with nulls
                for (i <- newInstructionsWithNulls.indices) {
                  newInstructionsWithNulls(i) = null
                }

                // Fill in actual instructions
                for ((pc, instruction) <- instructions) {
                  newInstructionsWithNulls(pc) = instruction
                }

                // Debugging: Print the instructions being passed to the new Code object
                println(s"Original Instructions for ${m.name}: ${originalBody.instructions.mkString(", ")}")
                println(s"New Instructions for ${m.name}: ${newInstructionsWithNulls.mkString(", ")}")

                val newBody = Code(
                  originalBody.maxStack,
                  originalBody.maxLocals,
                  newInstructionsWithNulls,
                  originalBody.exceptionHandlers,
                  originalBody.attributes)

                println(s"New body for method ${m.name}: $newBody")

                val result = m.copy(body = Some(newBody))

               val it = result.body.get.iterator
                val n = it.next()
                val n1 = it.next()
                print(n.toString + n1.toString)

                result
              case None =>
                println(s"Warning: No bytecode found for method ${m.name}. Keeping original method body.")
                m.copy()
            }
        }
      }

    }
     val newRawCF = Assembler(toDA(cf.copy(methods = newMethods)))
     val assembledMyIntfPath = Paths.get("HelloWorldToString.class")
     println("Created class file: "+Files.write(assembledMyIntfPath, newRawCF).toAbsolutePath)

     // Let's see the old class file...
     val odlCFHTML = ClassFile(in).head.toXHTML(None)
     val oldCFHTMLFile = writeAndOpen(odlCFHTML, "HelloWorldToString", ".html")
     println("original: " + oldCFHTMLFile)

     // Let's see the new class file...
     val newCF = ClassFile(() => new ByteArrayInputStream(newRawCF)).head.toXHTML(None)
     println("instrumented: " + writeAndOpen(newCF, "NewHelloWorldToString", ".html"))

    //println("Class file GeneratedHelloWorldToStringDALEQUEE.class has been generated." + newClass)
    // Let's test that the new class does what it is expected to do... (we execute the
    // instrumented method)
    val cl = new InMemoryClassLoader(Map((TheType.toJava, newRawCF)))
    val newClass = cl.findClass(TheType.toJava)
    //val instance = newClass.getDeclaredConstructor().newInstance()

    newClass.getMethod("main", (Array[String]()).getClass).invoke(null,null)
   }

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
            switchCases += ((pair._1, pair._2)) //case values to jump target
          }
          tacTargetToByteCodePcs += ((defaultTarget, currentPC))
          currentPC = StmtProcessor.processSwitch(defaultTarget, index, npairs, generatedByteCodeWithPC, currentPC)
        case VirtualMethodCall(_, declaringClass, isInterface, name, descriptor, receiver, params) =>
          tacTargetToByteCodePcs += ((-1, currentPC))
          currentPC = StmtProcessor.processVirtualMethodCall(declaringClass, isInterface, name, descriptor, receiver, params, generatedByteCodeWithPC, currentPC)
        case NonVirtualMethodCall(_, declaringClass, isInterface, name, descriptor, receiver, params) =>
          tacTargetToByteCodePcs += ((-1, currentPC))
          currentPC = StmtProcessor.processNonVirtualMethodCall(declaringClass, isInterface, name, descriptor, receiver, params, generatedByteCodeWithPC, currentPC)
        case StaticMethodCall(_, declaringClass, isInterface, name, descriptor, params) =>
          tacTargetToByteCodePcs += ((-1, currentPC))
          currentPC = StmtProcessor.processStaticMethodCall(declaringClass, isInterface, name, descriptor, params, generatedByteCodeWithPC, currentPC)
        case InvokedynamicMethodCall(_, bootstrapMethod, name, descriptor, params) =>
          tacTargetToByteCodePcs += ((-1, currentPC))
          currentPC = StmtProcessor.processInvokeDynamicMethodCall(bootstrapMethod, name, descriptor, params)
        case Checkcast(_, value, cmpTpe) =>
          tacTargetToByteCodePcs += ((-1, currentPC))
          currentPC = StmtProcessor.processCheckCast(value, cmpTpe, generatedByteCodeWithPC, currentPC)
        case Ret(_, returnAddresses) =>
          tacTargetToByteCodePcs += ((-1, currentPC))
          currentPC = StmtProcessor.processRet(returnAddresses, generatedByteCodeWithPC, currentPC)
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
            val updatedMatchOffsets = matchOffsets.map { case IntIntPair(caseValue, _) =>
              val tacTarget = findTacTarget(switchCases, caseValue)
              IntIntPair(caseValue, updateSwitchTargets(tacTargetToByteCodePcs, tacTarget))
            }
            val updatedDefaultOffset = updateSwitchTargets(tacTargetToByteCodePcs, defaultOffset)
            LOOKUPSWITCH(updatedDefaultOffset, updatedMatchOffsets)
          case TABLESWITCH(defaultOffset, low, high, jumpOffsets) =>
            val updatedJumpOffsets = jumpOffsets.zipWithIndex.map { case (_, index) =>
              val tacTarget = findTacTarget(switchCases, index)
              updateSwitchTargets(tacTargetToByteCodePcs, tacTarget)
            }
            val updatedDefaultOffset = updateSwitchTargets(tacTargetToByteCodePcs, defaultOffset)
            TABLESWITCH(updatedDefaultOffset, low, high, updatedJumpOffsets.to(ArraySeq))
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
}
