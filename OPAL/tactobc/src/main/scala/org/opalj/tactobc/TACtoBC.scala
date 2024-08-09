/* BSD 2-Clause License - see OPAL/LICENSE for details. */
package org.opalj.tactobc


import org.opalj.br.Method

import scala.Console.println
import org.opalj.br.analyses.Project
import org.opalj.br.instructions.Instruction

import org.opalj.tac._
import org.opalj.value.ValueInformation

import java.io.File
import scala.collection.mutable.ArrayBuffer

object TACtoBC {

  def main(args: Array[String]): Unit = {
    if (args.length != 2) {
      println("Usage: ListClassFiles <path to input class files directory> <path to output class files directory>")
      return
    }

    val inputDirPath = args(0)
    val outputDirPath = args(1)
    val inputDir = new File(inputDirPath)
    if (!inputDir.exists() || !inputDir.isDirectory) {
      println(s"Directory ${inputDir.getPath} does not exist or is not a directory.")
      return
    }

    val outputDir = new File(outputDirPath)
    if (!outputDir.exists()) {
      outputDir.mkdirs()
    }

    val classFiles = listClassFiles(inputDir)
    classFiles.foreach {
      classfile =>
        //todo: figure out how to get the input stream of the file
        //(1) compile bytecode
        compileByteCode(classfile)
        //(2) compile tac
        val tacs = compileTAC(classfile)
        // Print out TAC
        tacs.foreach { case (method, tac) =>
          tac.detach()
          println(s"Method: ${method.toJava}")
          println(tac.toString)
          println("\n")
        }
        //(3) generate bc from compiled tac
        // > Print out the translation from TAC to Bytecode
        val byteCodes = translateTACtoBC(tacs)
        byteCodes.foreach { case (method, bytecode) =>
          println(s"Method: ${method.toJava}")
          bytecode.foreach(instr => println(instr.toString))
        }
        //(4) generate .class files from translation
        val p = Project(classfile)
        ClassFileGenerator.generateClassFiles(byteCodes, p, inputDirPath, outputDirPath, classfile.getName)
        // println(classfile.getAbsolutePath)))
    }
  }

  def listClassFiles(directory: File): List[File] = {
    directory.listFiles().toList.filter(_.getName.endsWith(".class"))
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
   * bytecode instructions, effectively reversing the process of TAC generation.
   *
   * @param tacs A Map containing the TAC representations of methods to be translated back to bytecode.
   * @return A Map associating each method with its newly generated bytecode, wrapped in OPAL's Code structure.
   */
  def translateTACtoBC(tacs: Map[Method, AITACode[TACMethodParameter, ValueInformation]]): Map[Method, ArrayBuffer[(Int, Instruction)]] = {
    tacs.map { case (method, tacCode) =>
      // Convert the TAC representation back to bytecode for each method
      val bytecodeInstructions = translateSingleTACtoBC(method, tacCode)
      method -> bytecodeInstructions
    }
  }

  /**
   * Converts the TAC Stmts of a single method into bytecode instructions.
   *
   * This helper method processes one method's TAC representation at a time, converting it into a sequence
   * of bytecode instructions. It handles various types of TAC statements and expressions, translating them
   * into their equivalent bytecode form.
   *
   * @param tac The TAC representation of a method to be converted into bytecode.
   * @return An array of bytecode instructions representing the method's functionality
   */
  def translateSingleTACtoBC(method: Method, tac: AITACode[TACMethodParameter, ValueInformation]): ArrayBuffer[(Int, Instruction)] = {
    val tacStmts = tac.stmts.zipWithIndex
    //first pass -> prepare the LVIndexes to map the Variable to Indexes
    FirstPass.prepareLVIndexes(method, tacStmts)
    //second pass -> generate Bytecode Instructions from TAC Stmts
    val generatedByteCodeWithPC = ArrayBuffer[(Int, Instruction)]()
    val tacTargetToByteCodePcs = ArrayBuffer[(Int, Int)]()
    val switchCases = ArrayBuffer[(Int, Int)]() // To store switch case targets
    SecondPass.translateStmtsToInstructions(tacStmts, generatedByteCodeWithPC, tacTargetToByteCodePcs, switchCases)
    //third pass -> this time through the translated bytecode to calculate the right branching targets
    ThirdPass.updateTargetsOfJumpInstructions(tacStmts, generatedByteCodeWithPC, tacTargetToByteCodePcs, switchCases)
  }
}