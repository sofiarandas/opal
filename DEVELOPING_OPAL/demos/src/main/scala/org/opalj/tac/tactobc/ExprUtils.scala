package org.opalj.tac.tactobc

import org.opalj.br.instructions.{GETFIELD, GETSTATIC, ILOAD, Instruction, LoadClass, LoadDouble, LoadFloat, LoadInt, LoadLong, LoadMethodHandle, LoadMethodType, LoadString}
import org.opalj.bytecode.BytecodeProcessingFailedException
import org.opalj.tac.{ClassConst, Const, DoubleConst, Expr, FloatConst, GetField, GetStatic, IntConst, LongConst, MethodHandleConst, MethodTypeConst, StringConst, Var}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object ExprUtils {

  // Map for variable indexing within methods
  private val variableIndexMap: mutable.Map[String, Int] = mutable.Map.empty
  private var nextAvailableIndex: Int = 0

  def processExpression(expr: Expr[_], instructionsWithPCs: ArrayBuffer[(Int, Instruction)], currentPC: Int): Int = {
    expr match {
      case const: Const => loadConstant(const, instructionsWithPCs, currentPC)
      case variable: Var[_] => loadVariable(variable, instructionsWithPCs, currentPC)
      case fieldExpr: Expr[_] if fieldExpr.isInstanceOf[GetField[_]] || fieldExpr.isInstanceOf[GetStatic] => handleFieldAccess(fieldExpr, instructionsWithPCs, currentPC)
      case _ =>
        throw new UnsupportedOperationException("Unsupported expression type" + expr)
    }
  }

  private def loadConstant(constExpr: Const, instructionsWithPCs: ArrayBuffer[(Int, Instruction)], currentPC: Int): Int = {
    val instruction = constExpr match {
      case IntConst(_, value) => LoadInt(value)
      case FloatConst(_, value) => LoadFloat(value)
      case ClassConst(_, value) => LoadClass(value)
      case StringConst(_, value) => LoadString(value)
      case MethodHandleConst(_, value) => LoadMethodHandle(value)
      case MethodTypeConst(_, value) => LoadMethodType(value)
      case DoubleConst(_, value) => LoadDouble(value)
      case LongConst(_, value) => LoadLong(value)
      //todo: figure out how and what LoadDynamic is
      //I think LDCDynamic is not an actual Instruction.
      /*case Assignment(_, _, DynamicConst(_, bootstrapMethod, name, descriptor)) =>
        val instruction = LoadDynamic(-1, bootstrapMethod, name, descriptor)
        instructionsWithPCs += ((currentPC, instruction))
        currentPC += instruction.length*/
      case _ =>
        //todo: check that this is the right exception to throw
        throw BytecodeProcessingFailedException(
          "unsupported constant value: " + constExpr
        )
    }
    instructionsWithPCs += ((currentPC, instruction))
    currentPC + instruction.length // Update and return the new program counter
  }

  private def loadVariable(variable: Var[_], instructionsWithPCs: ArrayBuffer[(Int, Instruction)], currentPC: Int): Int = {
    val index = getVariableIndex(variable.name)
    val instruction = ILOAD(index) // Simplification for demonstration
    instructionsWithPCs += ((currentPC, instruction))
    currentPC + 1 // Update and return the new program counter
  }

  private def getVariableIndex(variableName: String): Int = {
    variableIndexMap.getOrElseUpdate(variableName, {
      val newIndex = nextAvailableIndex
      nextAvailableIndex += 1
      newIndex
    })
  }

  private def handleFieldAccess(fieldExpr: Expr[_], instructionsWithPCs: ArrayBuffer[(Int, Instruction)], currentPC: Int): Int = {
    val instruction = fieldExpr match {
      case getFieldExpr: GetField[_] =>
        GETFIELD(getFieldExpr.declaringClass, getFieldExpr.name, getFieldExpr.declaredFieldType)
      case getStaticExpr: GetStatic =>
        GETSTATIC(getStaticExpr.declaringClass, getStaticExpr.name, getStaticExpr.declaredFieldType)
      case _ => throw new IllegalArgumentException("Expected a field access expression")
    }
    instructionsWithPCs += ((currentPC, instruction))
    currentPC + instruction.length // Update and return the new program counter
  }
}
