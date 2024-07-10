/* BSD 2-Clause License - see OPAL/LICENSE for details. */
package org.opalj.tac.tactobc

import org.opalj.BinaryArithmeticOperators.{Add, And, Divide, Modulo, Multiply, Or, ShiftLeft, ShiftRight, Subtract, UnsignedShiftRight, XOr}
import org.opalj.br.{ComputationalTypeDouble, ComputationalTypeFloat, ComputationalTypeInt, ComputationalTypeLong, ComputationalTypeReference, ObjectType}
import org.opalj.br.instructions.{ALOAD, ALOAD_0, ALOAD_1, ALOAD_2, ALOAD_3, ASTORE, ASTORE_0, ASTORE_1, ASTORE_2, ASTORE_3, BIPUSH, DADD, DCONST_0, DCONST_1, DDIV, DLOAD, DLOAD_0, DLOAD_1, DLOAD_2, DLOAD_3, DMUL, DREM, DSTORE, DSTORE_0, DSTORE_1, DSTORE_2, DSTORE_3, DSUB, FADD, FCONST_0, FCONST_1, FCONST_2, FDIV, FLOAD, FLOAD_0, FLOAD_1, FLOAD_2, FLOAD_3, FMUL, FREM, FSTORE, FSTORE_0, FSTORE_1, FSTORE_2, FSTORE_3, FSUB, GETFIELD, GETSTATIC, IADD, IAND, ICONST_0, ICONST_1, ICONST_2, ICONST_3, ICONST_4, ICONST_5, ICONST_M1, IDIV, ILOAD, ILOAD_0, ILOAD_1, ILOAD_2, ILOAD_3, IMUL, INVOKEINTERFACE, INVOKESTATIC, INVOKEVIRTUAL, IOR, IREM, ISHL, ISHR, ISTORE, ISTORE_0, ISTORE_1, ISTORE_2, ISTORE_3, ISUB, IUSHR, IXOR, Instruction, LADD, LAND, LCONST_0, LCONST_1, LDIV, LLOAD, LLOAD_0, LLOAD_1, LLOAD_2, LLOAD_3, LMUL, LOR, LREM, LSHL, LSHR, LSTORE, LSTORE_0, LSTORE_1, LSTORE_2, LSTORE_3, LSUB, LUSHR, LXOR, LoadClass, LoadDouble, LoadFloat, LoadInt, LoadLong, LoadMethodHandle, LoadMethodType, LoadString, NEW, SIPUSH}
import org.opalj.bytecode.BytecodeProcessingFailedException
import org.opalj.tac.{BinaryExpr, ClassConst, Const, DVar, DoubleConst, Expr, FloatConst, GetField, GetStatic, IntConst, LongConst, MethodHandleConst, MethodTypeConst, New, StaticFunctionCall, StringConst, UVar, Var, VirtualFunctionCall}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object ExprUtils {


  def processExpression(expr: Expr[_], instructionsWithPCs: ArrayBuffer[(Int, Instruction)], currentPC: Int): Int = {
    expr match {
      case const: Const => loadConstant(const, instructionsWithPCs, currentPC)
      case variable: Var[_] => loadVariable(variable, instructionsWithPCs, currentPC)
      case fieldExpr: Expr[_] if fieldExpr.isInstanceOf[GetField[_]] || fieldExpr.isInstanceOf[GetStatic] => handleFieldAccess(fieldExpr, instructionsWithPCs, currentPC)
      case binaryExpr: BinaryExpr[_] => handleBinaryExpr(binaryExpr, instructionsWithPCs, currentPC)
      case virtualFunctionCallExpr: VirtualFunctionCall[_] => handleVirtualFunctionCall(virtualFunctionCallExpr, instructionsWithPCs, currentPC)
      case staticFunctionCallExpr: StaticFunctionCall[_] => handleStaticFunctionCall(staticFunctionCallExpr, instructionsWithPCs, currentPC)
      case newExpr: New => handleNewExpr(newExpr.tpe, instructionsWithPCs, currentPC)
      case _ =>
        throw new UnsupportedOperationException("Unsupported expression type" + expr)
    }
  }

  def handleNewExpr(tpe: ObjectType, instructionsWithPCs: ArrayBuffer[(Int, Instruction)], currentPC: Int): Int = {
    val instruction = NEW(tpe)
    instructionsWithPCs += ((currentPC, instruction))
    currentPC + instruction.length
  }

  def handleStaticFunctionCall(expr: StaticFunctionCall[_], instructionsWithPCs: ArrayBuffer[(Int, Instruction)], currentPC: Int): Int = {
    val instruction = if (expr.isInterface) {
      INVOKEINTERFACE(expr.declaringClass, expr.name, expr.descriptor)
      throw new UnsupportedOperationException("Unsupported expression type" + expr)
    } else {
      INVOKESTATIC(expr.declaringClass, expr.isInterface, expr.name, expr.descriptor)
    }

    instructionsWithPCs += ((currentPC, instruction))
    currentPC + instruction.length
  }

  def handleVirtualFunctionCall(expr: VirtualFunctionCall[_], instructionsWithPCs: ArrayBuffer[(Int, Instruction)], currentPC: Int): Int = {
    // Process the receiver object (e.g., aload_0 for `this`)
    val afterReceiverPC = ExprUtils.processExpression(expr.receiver, instructionsWithPCs, currentPC)

    // Initialize the PC after processing the receiver
    var currentAfterParamsPC = afterReceiverPC

    // Process each parameter and update the PC accordingly
    for (param <- expr.params) {
      currentAfterParamsPC = ExprUtils.processExpression(param, instructionsWithPCs, currentAfterParamsPC)
    }
    val instruction = if (expr.isInterface) {
      //INVOKEINTERFACE(expr.declaringClass, expr.name, expr.descriptor)
      throw new UnsupportedOperationException("Unsupported expression type" + expr)
    } else {
      INVOKEVIRTUAL(expr.declaringClass, expr.name, expr.descriptor)
    }

    instructionsWithPCs += ((currentAfterParamsPC, instruction))
    currentAfterParamsPC + instruction.length
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
  private val variableLvlIndexMap: mutable.Map[Int, Int] = mutable.Map.empty
  private var nextAvailableIndex: Int = 1
  // Reserve index 0 for level 0
  //variableLvlIndexMap(0) = 0
  // To handle unique levels for DVar with origin 0
  //private var dVarZeroLevel: Int = 1

  def getVariableLvlIndex(variable: Var[_]): Int = {
    val lvl = getVariableLvl(variable)
    variableLvlIndexMap.getOrElseUpdate(lvl, {
        val newIndex = nextAvailableIndex
        nextAvailableIndex += 1
        newIndex
      })
  }

  // Determine the level of a given variable
  def getVariableLvl(variable: Var[_]): Int = {
    val result = variable match {
      case uVar: UVar[_] => if(uVar.definedBy.toList.last > 0) {
        uVar.definedBy.toList.last
      }else 0
      case dVar : DVar[_] =>
        // Use the origin for DVar, with special handling for origin 0
        /*if (dVar.origin == 0) {
          val currentLevel = dVarZeroLevel
          dVarZeroLevel += 1
          currentLevel*/
        //} else {
          dVar.origin
        //}
      case _ => -1
    }
    result
  }

  def loadVariable(variable: Var[_], instructionsWithPCs: ArrayBuffer[(Int, Instruction)], currentPC: Int): Int = {
    val index = getVariableLvlIndex(variable)
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
  }

  def storeVariable(variable: Var[_], instructionsWithPCs: ArrayBuffer[(Int, Instruction)], currentPC: Int): Int = {
    //val variableName = handleDVarName(variable)
    val index = getVariableLvlIndex(variable)
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

  def handleBinaryExpr(binaryExpr: BinaryExpr[_], instructionsWithPCs: ArrayBuffer[(Int, Instruction)], currentPC: Int): Int = {
    // process the left expr and save the pc to give in the right expr processing
    val leftPC = processExpression(binaryExpr.left, instructionsWithPCs, currentPC)
    // process the right Expr
    val rightPC = processExpression(binaryExpr.right, instructionsWithPCs, leftPC)
    val (instruction, instructionLength) = (binaryExpr.cTpe, binaryExpr.op) match {
      //Double
      case (ComputationalTypeDouble, Add) => (DADD, DADD.length)
      case (ComputationalTypeDouble, Subtract) => (DSUB, DSUB.length)
      case (ComputationalTypeDouble, Multiply) => (DMUL, DMUL.length)
      case (ComputationalTypeDouble, Divide) => (DDIV, DDIV.length)
      case (ComputationalTypeDouble, Modulo) => (DREM, DREM.length)
      //Todo figure out where and how to do with Negate
      //Float
      case (ComputationalTypeFloat, Add) => (FADD, FADD.length)
      case (ComputationalTypeFloat, Subtract) => (FSUB, FSUB.length)
      case (ComputationalTypeFloat, Multiply) => (FMUL, FMUL.length)
      case (ComputationalTypeFloat, Divide) => (FDIV, FDIV.length)
      case (ComputationalTypeFloat, Modulo) => (FREM, FREM.length)
      //Int
      case (ComputationalTypeInt, Add) => (IADD, IADD.length)
      case (ComputationalTypeInt, Subtract) => (ISUB, ISUB.length)
      case (ComputationalTypeInt, Multiply) => (IMUL, IMUL.length)
      case (ComputationalTypeInt, Divide) => (IDIV, IDIV.length)
      case (ComputationalTypeInt, Modulo) => (IREM, IREM.length)
      case (ComputationalTypeInt, And) => (IAND, IAND.length)
      case (ComputationalTypeInt, Or) => (IOR, IOR.length)
      case (ComputationalTypeInt, ShiftLeft) => (ISHL, ISHL.length)
      case (ComputationalTypeInt, ShiftRight) => (ISHR, ISHR.length)
      case (ComputationalTypeInt, UnsignedShiftRight) => (IUSHR, IUSHR.length)
      case (ComputationalTypeInt, XOr) => (IXOR, IXOR.length)
      //Long
      case (ComputationalTypeLong, Add) => (LADD, LADD.length)
      case (ComputationalTypeLong, Subtract) => (LSUB, LSUB.length)
      case (ComputationalTypeLong, Multiply) => (LMUL, LMUL.length)
      case (ComputationalTypeLong, Divide) => (LDIV, LDIV.length)
      case (ComputationalTypeLong, Modulo) => (LREM, LREM.length)
      case (ComputationalTypeLong, And) => (LAND, LAND.length)
      case (ComputationalTypeLong, Or) => (LOR, LOR.length)
      case (ComputationalTypeLong, ShiftLeft) => (LSHL, LSHL.length)
      case (ComputationalTypeLong, ShiftRight) => (LSHR, LSHR.length)
      case (ComputationalTypeLong, UnsignedShiftRight) => (LUSHR, LUSHR.length)
      case (ComputationalTypeLong, XOr) => (LXOR, LXOR.length)
      //Unsupported
      case _ => throw new UnsupportedOperationException("Unsupported operation or computational type in BinaryExpr" + binaryExpr)
    }
    val offsetPC = currentPC + (rightPC - currentPC)
    instructionsWithPCs += ((offsetPC, instruction))
    offsetPC + instructionLength
  }
}
