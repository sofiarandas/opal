package org.opalj.tac.tactobc

import org.opalj.BinaryArithmeticOperators.{Add, And, Divide, Modulo, Multiply, Or, ShiftLeft, ShiftRight, Subtract, UnsignedShiftRight, XOr}
import org.opalj.br.{ComputationalTypeDouble, ComputationalTypeFloat, ComputationalTypeInt, ComputationalTypeLong, ComputationalTypeReference}
import org.opalj.br.instructions.{ALOAD, ASTORE, DADD, DDIV, DLOAD, DMUL, DREM, DSTORE, DSUB, FADD, FDIV, FLOAD, FMUL, FREM, FSTORE, FSUB, GETFIELD, GETSTATIC, IADD, IAND, IDIV, ILOAD, IMUL, IOR, IREM, ISHL, ISHR, ISTORE, ISUB, IUSHR, IXOR, Instruction, LADD, LDIV, LLOAD, LMUL, LREM, LSTORE, LSUB, LoadClass, LoadDouble, LoadFloat, LoadInt, LoadLong, LoadMethodHandle, LoadMethodType, LoadString}
import org.opalj.bytecode.BytecodeProcessingFailedException
import org.opalj.tac.{BinaryExpr, ClassConst, Const, DoubleConst, Expr, FloatConst, GetField, GetStatic, IntConst, LongConst, MethodHandleConst, MethodTypeConst, StringConst, Var}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object ExprUtils {

  def processExpression(expr: Expr[_], instructionsWithPCs: ArrayBuffer[(Int, Instruction)], currentPC: Int): Int = {
    expr match {
      case const: Const => loadConstant(const, instructionsWithPCs, currentPC)
      case variable: Var[_] =>
        loadVariable(variable, instructionsWithPCs, currentPC)
      case fieldExpr: Expr[_] if fieldExpr.isInstanceOf[GetField[_]] || fieldExpr.isInstanceOf[GetStatic] => handleFieldAccess(fieldExpr, instructionsWithPCs, currentPC)
      case binaryExpr: BinaryExpr[_] => handleBinaryExpr(binaryExpr, instructionsWithPCs, currentPC)
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

  // Map for variable indexing within methods
  private val variableIndexMap: mutable.Map[String, Int] = mutable.Map.empty
  private var nextAvailableIndex: Int = 0

  private def getVariableIndex(variableName: String): Int = {
    variableIndexMap.getOrElseUpdate(variableName, {
      val newIndex = nextAvailableIndex
      nextAvailableIndex += 1
      newIndex
    })
  }

  private def loadVariable(variable: Var[_], instructionsWithPCs: ArrayBuffer[(Int, Instruction)], currentPC: Int): Int = {
    val index = getVariableIndex(variable.name)
    val instruction = variable.cTpe match {
      case ComputationalTypeInt => ILOAD(index)
      case ComputationalTypeFloat => FLOAD(index)
      case ComputationalTypeDouble => DLOAD(index)
      case ComputationalTypeLong => LLOAD(index)
      case ComputationalTypeReference => ALOAD(index)
      case _ => throw new UnsupportedOperationException("Unsupported computational type for loading variable" + variable)
    }
    instructionsWithPCs += ((currentPC, instruction))
    currentPC + 1 // Update and return the new program counter
    //Todo handle IINC here(?)
  }

  def storeVariable(variable: Var[_], instructionsWithPCs: ArrayBuffer[(Int, Instruction)], currentPC: Int): Int = {
    val index = getVariableIndex(variable.name)
    val storeInstruction = variable.cTpe match {
      case ComputationalTypeInt => ISTORE(index)
      case ComputationalTypeFloat => FSTORE(index)
      case ComputationalTypeDouble => DSTORE(index)
      case ComputationalTypeLong => LSTORE(index)
      case ComputationalTypeReference => ASTORE(index)
      case _ => throw new UnsupportedOperationException("Unsupported computational type for storing variable" + variable)
    }
    instructionsWithPCs += ((currentPC, storeInstruction))
    currentPC + 1  // Assuming each store instruction is 1 byte
  }

  private def handleFieldAccess(fieldExpr: Expr[_], instructionsWithPCs: ArrayBuffer[(Int, Instruction)], currentPC: Int): Int = {
    val instruction = fieldExpr match {
      case getFieldExpr: GetField[_] =>
        GETFIELD(getFieldExpr.declaringClass, getFieldExpr.name, getFieldExpr.declaredFieldType)
      case getStaticExpr: GetStatic =>
        GETSTATIC(getStaticExpr.declaringClass, getStaticExpr.name, getStaticExpr.declaredFieldType)
      case _ => throw new IllegalArgumentException("Expected a field access expression" + fieldExpr)
    }
    instructionsWithPCs += ((currentPC, instruction))
    currentPC + instruction.length // Update and return the new program counter
  }

  private def handleBinaryExpr(binaryExpr: BinaryExpr[_], instructionsWithPCs: ArrayBuffer[(Int, Instruction)], currentPC: Int): Int = {
    // Assuming the left and right are simple vars or consts
    //save the PC offset of the left Expr to know where to continue with the right Expr
    val leftPC = processExpression(binaryExpr.left, instructionsWithPCs, currentPC)
    //process the right Expr
    processExpression(binaryExpr.right, instructionsWithPCs, leftPC)
    val instruction = (binaryExpr.cTpe, binaryExpr.op) match {
      //Double
      case (ComputationalTypeDouble, Add) => DADD
      case (ComputationalTypeDouble, Subtract) => DSUB
      case (ComputationalTypeDouble, Multiply) => DMUL
      case (ComputationalTypeDouble, Divide) => DDIV
      case (ComputationalTypeDouble, Modulo) => DREM
      //Todo figure out where and how to do with Negate
      //Float
      case (ComputationalTypeFloat, Add) => FADD
      case (ComputationalTypeFloat, Subtract) => FSUB
      case (ComputationalTypeFloat, Multiply) => FMUL
      case (ComputationalTypeFloat, Divide) => FDIV
      case (ComputationalTypeFloat, Modulo) => FREM
      //Int
      case (ComputationalTypeInt, Add) => IADD
      case (ComputationalTypeInt, Subtract) => ISUB
      case (ComputationalTypeInt, Multiply) => IMUL
      case (ComputationalTypeInt, Divide) => IDIV
      case (ComputationalTypeInt, Modulo) => IREM
      case (ComputationalTypeInt, And) => IAND
      case (ComputationalTypeInt, Or) => IOR
      case (ComputationalTypeInt, ShiftLeft) => ISHL
      case (ComputationalTypeInt, ShiftRight) => ISHR
      case (ComputationalTypeInt, UnsignedShiftRight) => IUSHR
      case (ComputationalTypeInt, XOr) => IXOR
      //Long
      case (ComputationalTypeLong, Add) => LADD
      case (ComputationalTypeLong, Subtract) => LSUB
      case (ComputationalTypeLong, Multiply) => LMUL
      case (ComputationalTypeLong, Divide) => LDIV
      case (ComputationalTypeLong, Modulo) => LREM
      case (ComputationalTypeLong, And) => IAND
      case (ComputationalTypeLong, Or) => IOR
      case (ComputationalTypeLong, ShiftLeft) => ISHL
      case (ComputationalTypeLong, ShiftRight) => ISHR
      case (ComputationalTypeLong, UnsignedShiftRight) => IUSHR
      case (ComputationalTypeLong, XOr) => IXOR
      //Unsupported
      case _ => throw new UnsupportedOperationException("Unsupported operation or computational type in BinaryExpr" + binaryExpr)
    }
    instructionsWithPCs += ((currentPC, instruction))
    currentPC + instruction.length // Update and return the new program counter
  }
}
