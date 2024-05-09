/* BSD 2-Clause License - see OPAL/LICENSE for details. */
package org.opalj.tac.tactobc

import org.opalj.BinaryArithmeticOperators.{Add, And, Divide, Modulo, Multiply, Or, ShiftLeft, ShiftRight, Subtract, UnsignedShiftRight, XOr}
import org.opalj.br.{ComputationalTypeDouble, ComputationalTypeFloat, ComputationalTypeInt, ComputationalTypeLong, ComputationalTypeReference}
import org.opalj.br.instructions.{ALOAD, ALOAD_0, ALOAD_1, ALOAD_2, ALOAD_3, ASTORE, ASTORE_0, ASTORE_1, ASTORE_2, ASTORE_3, BIPUSH, DADD, DCONST_0, DCONST_1, DDIV, DLOAD, DLOAD_0, DLOAD_1, DLOAD_2, DLOAD_3, DMUL, DREM, DSTORE, DSTORE_0, DSTORE_1, DSTORE_2, DSTORE_3, DSUB, FADD, FCONST_0, FCONST_1, FCONST_2, FDIV, FLOAD, FLOAD_0, FLOAD_1, FLOAD_2, FLOAD_3, FMUL, FREM, FSTORE, FSTORE_0, FSTORE_1, FSTORE_2, FSTORE_3, FSUB, GETFIELD, GETSTATIC, IADD, IAND, ICONST_0, ICONST_1, ICONST_2, ICONST_3, ICONST_4, ICONST_5, ICONST_M1, IDIV, ILOAD, ILOAD_0, ILOAD_1, ILOAD_2, ILOAD_3, IMUL, IOR, IREM, ISHL, ISHR, ISTORE, ISTORE_0, ISTORE_1, ISTORE_2, ISTORE_3, ISUB, IUSHR, IXOR, Instruction, LADD, LCONST_0, LCONST_1, LDIV, LLOAD, LLOAD_0, LLOAD_1, LLOAD_2, LLOAD_3, LMUL, LREM, LSTORE, LSTORE_0, LSTORE_1, LSTORE_2, LSTORE_3, LSUB, LoadClass, LoadDouble, LoadFloat, LoadInt, LoadLong, LoadMethodHandle, LoadMethodType, LoadString, SIPUSH}
import org.opalj.bytecode.BytecodeProcessingFailedException
import org.opalj.tac.{BinaryExpr, ClassConst, Const, DoubleConst, Expr, FloatConst, GetField, GetStatic, IntConst, LongConst, MethodHandleConst, MethodTypeConst, StringConst, Var}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object ExprUtils {

  def processExpression(expr: Expr[_], instructionsWithPCs: ArrayBuffer[(Int, Instruction)], currentPC: Int): Int = {
    expr match {
      case const: Const => loadConstant(const, instructionsWithPCs, currentPC)
      case variable: Var[_] => loadVariable(variable, instructionsWithPCs, currentPC)
      case fieldExpr: Expr[_] if fieldExpr.isInstanceOf[GetField[_]] || fieldExpr.isInstanceOf[GetStatic] => handleFieldAccess(fieldExpr, instructionsWithPCs, currentPC)
      case binaryExpr: BinaryExpr[_] => handleBinaryExpr(binaryExpr, instructionsWithPCs, currentPC)
      case _ =>
        throw new UnsupportedOperationException("Unsupported expression type" + expr)
    }
  }

  private def loadConstant(constExpr: Const, instructionsWithPCs: ArrayBuffer[(Int, Instruction)], currentPC: Int): Int = {
    val instruction = constExpr match {
      case IntConst(_, value) => value match {
        case -1 => ICONST_M1
        case 0 => ICONST_0
        case 1 => ICONST_1
        case 2 => ICONST_2
        case 3 => ICONST_3
        case 4 => ICONST_4
        case 5 => ICONST_5
        case _ if value >= Byte.MinValue && value <= Byte.MaxValue => BIPUSH(value)
        case _ if value >= Short.MinValue && value <= Short.MaxValue => SIPUSH(value)
        case _ => LoadInt(value)
      }
      case FloatConst(_, value) => value match {
        case 0 => FCONST_0
        case 1 => FCONST_1
        case 2 => FCONST_2
        case _ => LoadFloat(value)
      }
      case ClassConst(_, value) => LoadClass(value)
      case StringConst(_, value) => LoadString(value)
      case MethodHandleConst(_, value) => LoadMethodHandle(value)
      case MethodTypeConst(_, value) => LoadMethodType(value)
      case DoubleConst(_, value) => value match {
        case 0 => DCONST_0
        case 1 => DCONST_1
        case _ => LoadDouble(value)
      }
      case LongConst(_, value) => value match {
        case 0 => LCONST_0
        case 1 => LCONST_1
        case _ => LoadLong(value)
      }
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
  private var nextAvailableIndex: Int = 1

  private def getVariableIndex(variableName: String): Int = {
    variableIndexMap.getOrElseUpdate(variableName, {
      val newIndex = nextAvailableIndex
      nextAvailableIndex += 1
      newIndex
    })
  }

  private def loadVariable(variable: Var[_], instructionsWithPCs: ArrayBuffer[(Int, Instruction)], currentPC: Int): Int = {
    val index = getVariableIndex(variable.name.drop(1).dropRight(1))
    val instruction = variable.cTpe match {
      case ComputationalTypeInt => index match {
        case 0 => ILOAD_0
        case 1 => ILOAD_1
        case 2 => ILOAD_2
        case 3 => ILOAD_3
        case _ => ILOAD(index)
      }
      case ComputationalTypeFloat => index match {
        case 0 => FLOAD_0
        case 1 => FLOAD_1
        case 2 => FLOAD_2
        case 3 => FLOAD_3
        case _ => FLOAD(index)
      }
      case ComputationalTypeDouble => index match {
        case 0 => DLOAD_0
        case 1 => DLOAD_1
        case 2 => DLOAD_2
        case 3 => DLOAD_3
        case _ => DLOAD(index)
      }
      case ComputationalTypeLong => index match {
        case 0 => LLOAD_0
        case 1 => LLOAD_1
        case 2 => LLOAD_2
        case 3 => LLOAD_3
        case _ => LLOAD(index)
      }
      case ComputationalTypeReference => index match {
        case 0 => ALOAD_0
        case 1 => ALOAD_1
        case 2 => ALOAD_2
        case 3 => ALOAD_3
        case _ => ALOAD(index)
      }
      case _ => throw new UnsupportedOperationException("Unsupported computational type for loading variable" + variable)
    }
    instructionsWithPCs += ((currentPC, instruction))
    currentPC + (if (index < 4) 1 else 2)
    //Todo handle IINC here(?)
  }

  def storeVariable(variable: Var[_], instructionsWithPCs: ArrayBuffer[(Int, Instruction)], currentPC: Int): Int = {
    val index = getVariableIndex(variable.name)
    val storeInstruction = variable.cTpe match {
      case ComputationalTypeInt => index match {
      //The <n> must be an index into the local variable array of the current frame (§2.6).
      // The value on the top of the operand stack must be of type int. It is popped from the operand stack, and the value of the local variable at <n> is set to value.
        case 0 => ISTORE_0
        case 1 => ISTORE_1
        case 2 => ISTORE_2
        case 3 => ISTORE_3
        case _ => ISTORE(index)
      }
      case ComputationalTypeFloat => index match {
        case 0 => FSTORE_0
        case 1 => FSTORE_1
        case 2 => FSTORE_2
        case 3 => FSTORE_3
        case _ => FSTORE(index)
      }
      case ComputationalTypeDouble => index match {
        case 0 => DSTORE_0
        case 1 => DSTORE_1
        case 2 => DSTORE_2
        case 3 => DSTORE_3
        case _ => DSTORE(index)
      }
      case ComputationalTypeLong => index match {
        case 0 => LSTORE_0
        case 1 => LSTORE_1
        case 2 => LSTORE_2
        case 3 => LSTORE_3
        case _ => LSTORE(index)
      }
      //todo: find out if I should handle this cases
      case ComputationalTypeReference => index match {
        case 0 => ASTORE_0
        case 1 => ASTORE_1
        case 2 => ASTORE_2
        case 3 => ASTORE_3
        case _ => ASTORE(index)
      }
      //todo: handle AASTORE
      case _ => throw new UnsupportedOperationException("Unsupported computational type for storing variable" + variable)
    }
    instructionsWithPCs += ((currentPC, storeInstruction))
    currentPC + (if (index < 4) 1 else 2)
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
    val rightPC = processExpression(binaryExpr.right, instructionsWithPCs, leftPC)
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
    val offsetPC = currentPC + (rightPC - currentPC)
    instructionsWithPCs += ((offsetPC, instruction))
    offsetPC + instruction.length // Update and return the new program counter
  }
}
