/* BSD 2-Clause License - see OPAL/LICENSE for details. */
package org.opalj.tactobc

import org.opalj.RelationalOperator
import org.opalj.RelationalOperators._
import org.opalj.br.{ArrayType, BootstrapMethod, ByteType, CharType, ComputationalTypeDouble, ComputationalTypeFloat, ComputationalTypeInt, ComputationalTypeLong, ComputationalTypeReference, DoubleType, FieldType, FloatType, IntegerType, LongType, MethodDescriptor, ObjectType, PCs, ReferenceType, ShortType}
import org.opalj.br.instructions.{AASTORE, ARETURN, ATHROW, BASTORE, CASTORE, CHECKCAST, DASTORE, DEFAULT_INVOKEDYNAMIC, DRETURN, FASTORE, FRETURN, GOTO, IASTORE, IFNONNULL, IFNULL, IF_ACMPEQ, IF_ACMPNE, IF_ICMPEQ, IF_ICMPGE, IF_ICMPGT, IF_ICMPLE, IF_ICMPLT, IF_ICMPNE, INVOKEINTERFACE, INVOKESPECIAL, INVOKESTATIC, INVOKEVIRTUAL, IRETURN, Instruction, JSR, LASTORE, LOOKUPSWITCH, LRETURN, MONITORENTER, MONITOREXIT, NOP, PUTFIELD, PUTSTATIC, RET, RETURN, SASTORE, TABLESWITCH}
import org.opalj.collection.immutable.{IntIntPair, IntTrieSet}
import org.opalj.tac.{Expr, UVar, Var}
import org.opalj.tactobc.ExprProcessor.inferElementType

import scala.collection.immutable.ArraySeq
import scala.collection.mutable.ArrayBuffer

object StmtProcessor {

  //Assignment
  def processAssignment(targetVar: Var[_], expr: Expr[_], instructionsWithPCs: ArrayBuffer[(Int, Instruction)], currentPC: Int): Int = {
    // Evaluate the RHS and update the PC accordingly
    val afterExprPC = ExprProcessor.processExpression(expr, instructionsWithPCs, currentPC)
    // Store the result into the target variable and update the PC
    val finalPC = ExprProcessor.storeVariable(targetVar, instructionsWithPCs, afterExprPC)
    // Return the updated PC
    finalPC
  }

  def processSwitch(defaultOffset: Int, index: Expr[_], npairs: ArraySeq[IntIntPair /*(Case Value, Jump Target)*/], instructionsWithPCs: ArrayBuffer[(Int, Instruction)], currentPC: Int): Int = {
    // Translate the index expression first
    val afterExprPC = ExprProcessor.processExpression(index, instructionsWithPCs, currentPC)

    // Prepare the bytecode pairs with placeholders for targets
    val bCnpairs = prepareBCnpairs(npairs)

    if (isLookupSwitch(index)) {
      // Add LOOKUPSWITCH instruction with placeholders for targets
      val lookupswitchInstruction = LOOKUPSWITCH(defaultOffset, bCnpairs)
      instructionsWithPCs += ((afterExprPC, lookupswitchInstruction))
      afterExprPC + lookupSwitchLength(bCnpairs.size, afterExprPC) - 1
    } else {
      // Add TABLESWITCH instruction with placeholders for targets
      val minValue = bCnpairs.minBy(_._1)._1
      val maxValue = bCnpairs.maxBy(_._1)._1
      val jumpTable = ArrayBuffer.fill(maxValue - minValue + 1)(-1)

      // Set the case values in the jump table
      bCnpairs.foreach { case IntIntPair(caseValue, _) =>
        jumpTable(caseValue - minValue) = -1
      }

      val tableswitchInstruction = TABLESWITCH(defaultOffset, minValue, maxValue, jumpTable.to(ArraySeq))
      instructionsWithPCs += ((afterExprPC, tableswitchInstruction))
      afterExprPC + tableSwitchLength(minValue, maxValue, afterExprPC) - 1
    }
  }

  def lookupSwitchLength(numPairs: Int, currentPC: Int): Int = {
    // Opcode (1 byte) + padding (0-3 bytes) + default offset (4 bytes) + number of pairs (4 bytes) + pairs (8 bytes each)
    val padding = (4 - (currentPC % 4)) % 4
    1 + padding + 4 + 4 + (numPairs * 8)
  }

  def tableSwitchLength(low: Int, high: Int, currentPC: Int): Int = {
    // Opcode (1 byte) + padding (0-3 bytes) + default offset (4 bytes) + low value (4 bytes) + high value (4 bytes) + jump offsets (4 bytes each)
    val padding = (4 - (currentPC % 4)) % 4
    val numOffsets = high - low + 1
    1 + padding + 4 + 4 + 4 + (numOffsets * 4)
  }

  def prepareBCnpairs(npairs: ArraySeq[IntIntPair]): ArraySeq[IntIntPair] = {
    npairs.map { case IntIntPair(caseValue, _) => IntIntPair(caseValue, -1) }
  }

  def isLookupSwitch(index: Expr[_]): Boolean = {
    index match {
      case variable: UVar[_] => variable.defSites.size == 1
      case _ => false
    }
  }

  def processReturn(instructionsWithPCs: ArrayBuffer[(Int, Instruction)], currentPC: Int): Int = {
    val instruction = RETURN
    instructionsWithPCs += ((currentPC, instruction))
    currentPC + instruction.length
  }

  def processReturnValue(expr: Expr[_], instructionsWithPCs: ArrayBuffer[(Int, Instruction)], currentPC: Int): Int = {
    val afterExprPC = ExprProcessor.processExpression(expr, instructionsWithPCs, currentPC)
    val instruction = expr.cTpe match {
      case ComputationalTypeInt => IRETURN
      case ComputationalTypeLong => LRETURN
      case ComputationalTypeFloat => FRETURN
      case ComputationalTypeDouble => DRETURN
      case ComputationalTypeReference => ARETURN
      case _ => throw new UnsupportedOperationException("Unsupported computational type:" + expr.cTpe)
    }
    instructionsWithPCs += ((afterExprPC, instruction))
    afterExprPC + instruction.length
  }

  def processVirtualMethodCall(declaringClass: ReferenceType, isInterface: Boolean, methodName: String, methodDescriptor: MethodDescriptor, receiver: Expr[_], params: Seq[Expr[_]], instructionsWithPCs: ArrayBuffer[(Int, Instruction)], currentPC: Int): Int = {
    // Process the receiver object (e.g., aload_0 for `this`)
    val afterReceiverPC = ExprProcessor.processExpression(receiver, instructionsWithPCs, currentPC)

    // Initialize the PC after processing the receiver
    var currentAfterParamsPC = afterReceiverPC

    // Process each parameter and update the PC accordingly
    for (param <- params) {
      currentAfterParamsPC = ExprProcessor.processExpression(param, instructionsWithPCs, currentAfterParamsPC)
    }

    val instruction = { if (isInterface) {
      INVOKEINTERFACE(declaringClass.asObjectType, methodName, methodDescriptor)
    }else
      INVOKEVIRTUAL(declaringClass, methodName, methodDescriptor)
    }
    instructionsWithPCs += ((currentAfterParamsPC, instruction))
    currentAfterParamsPC + instruction.length
  }

  def processArrayStore(arrayRef: Expr[_], index: Expr[_], value: Expr[_], instructionsWithPCs: ArrayBuffer[(Int, Instruction)], currentPC: Int): Int = {
    // Load the arrayRef onto the stack
    val pcAfterArrayRefLoad = ExprProcessor.processExpression(arrayRef, instructionsWithPCs, currentPC)

    // Load the index onto the stack
    val pcAfterIndexLoad = ExprProcessor.processExpression(index, instructionsWithPCs, pcAfterArrayRefLoad)

    // Load the value to be stored onto the stack
    val pcAfterValueLoad = ExprProcessor.processExpression(value, instructionsWithPCs, pcAfterIndexLoad)

    // Infer the element type from the array reference expression
    val elementType = inferElementType(arrayRef)


    val instruction = elementType match {
      case IntegerType => IASTORE
      case LongType => LASTORE
      case FloatType => FASTORE
      case DoubleType => DASTORE
      case ByteType => BASTORE
      case CharType => CASTORE
      case ShortType => SASTORE
      case _: ObjectType => AASTORE
      case ArrayType(componentType) => componentType match {
        case _: ReferenceType => AASTORE
        case _: CharType => CASTORE
        case _: FloatType => FASTORE
        case _: DoubleType => DASTORE
        case _: ByteType => BASTORE
        case _: ShortType => SASTORE
        case _: IntegerType => IASTORE
        case _: LongType => LASTORE
        case _ => throw new IllegalArgumentException(s"Unsupported array store type $componentType")
      }
      case _ => throw new IllegalArgumentException(s"Unsupported array store type $elementType")
    }
    // Add the store instruction
    instructionsWithPCs += ((pcAfterValueLoad, instruction))
    pcAfterValueLoad + instruction.length
  }

  def processNop(instructionsWithPCs: ArrayBuffer[(Int, Instruction)], currentPC: Int): Int = {
    val instruction = NOP
    instructionsWithPCs += ((currentPC, instruction))
    currentPC + instruction.length
  }

  def processInvokeDynamicMethodCall(bootstrapMethod: BootstrapMethod, name: String, descriptor: MethodDescriptor, params: Seq[Expr[_]], instructionsWithPCs: ArrayBuffer[(Int, Instruction)], currentPC: Int): Int = {
    var currentAfterParamsPC = currentPC
    for (param <- params) {
      currentAfterParamsPC = ExprProcessor.processExpression(param, instructionsWithPCs, currentAfterParamsPC)
    }
    val invokeDynamicInstruction = DEFAULT_INVOKEDYNAMIC(bootstrapMethod, name, descriptor)
    instructionsWithPCs += ((currentAfterParamsPC, invokeDynamicInstruction))
    currentAfterParamsPC + invokeDynamicInstruction.length
  }

  def processCheckCast(value: Expr[_], cmpTpe: ReferenceType, instructionsWithPCs: ArrayBuffer[(Int, Instruction)], currentPC: Int): Int = {
    val nextPC = ExprProcessor.processExpression(value, instructionsWithPCs, currentPC)
    val instruction = CHECKCAST(cmpTpe)
    instructionsWithPCs += ((nextPC, instruction))
    nextPC + instruction.length
  }

  def processRet(returnAddresses: PCs, instructionsWithPCs: ArrayBuffer[(Int, Instruction)], currentPC: Int): Int = {
    // Ensure there is only one return address, as RET can only work with one local variable index
    if (returnAddresses.size != 1) {
      throw new IllegalArgumentException(s"RET instruction expects exactly one return address, but got: ${returnAddresses.size}")
    }

    // The RET instruction requires the index of the local variable that holds the return address
    val localVarIndex = returnAddresses.head

    // Create the RET instruction with the correct local variable index
    val instruction = RET(localVarIndex)
    instructionsWithPCs += ((currentPC, instruction))

    // Return the next program counter
    currentPC + instruction.length
  }

  def processCaughtException(exceptionType: Option[ObjectType], throwingStmts: IntTrieSet, instructionsWithPCs: ArrayBuffer[(Int, Instruction)], currentPC: Int): Int = {
    //todo: handle this correctly
    1
  }

  def processThrow(exception: Expr[_], instructionsWithPCs: ArrayBuffer[(Int, Instruction)], currentPC: Int): Int = {
    val pcAfterException = ExprProcessor.processExpression(exception, instructionsWithPCs, currentPC)
    val instruction = ATHROW
    instructionsWithPCs += ((pcAfterException, instruction))
    pcAfterException + 1
  }

  def processPutStatic(declaringClass: ObjectType, name: String, declaredFieldType: FieldType, value: Expr[_], instructionsWithPCs: ArrayBuffer[(Int, Instruction)], currentPC: Int): Int = {
    val pcAfterValueExpr = ExprProcessor.processExpression(value, instructionsWithPCs, currentPC)
    val instruction = PUTSTATIC(declaringClass, name, declaredFieldType)
    instructionsWithPCs += ((pcAfterValueExpr, instruction))
    pcAfterValueExpr + instruction.length
  }

  def processPutField(declaringClass: ObjectType, name: String, declaredFieldType: FieldType, objRef: Expr[_], value: Expr[_], instructionsWithPCs: ArrayBuffer[(Int, Instruction)], currentPC: Int): Int = {
    // Load the object reference onto the stack
    val pcAfterObjRefLoad = ExprProcessor.processExpression(objRef, instructionsWithPCs, currentPC)
    // Load the value to be stored onto the stack
    val pcAfterValueLoad = ExprProcessor.processExpression(value, instructionsWithPCs, pcAfterObjRefLoad)
    val instruction = PUTFIELD(declaringClass, name, declaredFieldType)
    instructionsWithPCs += ((pcAfterValueLoad, instruction))
    pcAfterValueLoad + instruction.length
  }

  def processMonitorEnter(objRef: Expr[_], instructionsWithPCs: ArrayBuffer[(Int, Instruction)], currentPC: Int): Int = {
    // Load the object reference onto the stack
    val pcAfterObjRefLoad = ExprProcessor.processExpression(objRef, instructionsWithPCs, currentPC)
    val instruction = MONITORENTER
    instructionsWithPCs += ((pcAfterObjRefLoad, instruction))
    pcAfterObjRefLoad + instruction.length
  }

  def processMonitorExit(objRef: Expr[_], instructionsWithPCs: ArrayBuffer[(Int, Instruction)], currentPC: Int): Int = {
    // Load the object reference onto the stack
    val pcAfterObjRefLoad = ExprProcessor.processExpression(objRef, instructionsWithPCs, currentPC)
    val instruction = MONITOREXIT
    instructionsWithPCs += ((pcAfterObjRefLoad, instruction))
    pcAfterObjRefLoad + instruction.length
  }

  def processJSR(instructionsWithPCs: ArrayBuffer[(Int, Instruction)], currentPC: Int): Int = {
    val instruction = JSR(-1)
    instructionsWithPCs += ((currentPC, instruction))
    currentPC + 1
  }

  def processNonVirtualMethodCall(declaringClass: ObjectType, isInterface: Boolean, methodName: String, methodDescriptor: MethodDescriptor, receiver: Expr[_], params: Seq[Expr[_]], instructionsWithPCs: ArrayBuffer[(Int, Instruction)], currentPC: Int): Int = {
    val afterReceiverPC = ExprProcessor.processExpression(receiver, instructionsWithPCs, currentPC)

    // Initialize the PC after processing the receiver
    var currentAfterParamsPC = afterReceiverPC

    // Process each parameter and update the PC accordingly
    for (param <- params) {
      currentAfterParamsPC = ExprProcessor.processExpression(param, instructionsWithPCs, currentAfterParamsPC)
    }
    val instruction = INVOKESPECIAL(declaringClass, isInterface, methodName, methodDescriptor)
    instructionsWithPCs += ((currentAfterParamsPC, instruction))
    currentAfterParamsPC + instruction.length
  }

  def processStaticMethodCall(declaringClass: ObjectType, isInterface: Boolean, methodName: String, methodDescriptor: MethodDescriptor, params: Seq[Expr[_]], instructionsWithPCs: ArrayBuffer[(Int, Instruction)], currentPC: Int): Int = {
    // Initialize the PC after processing the receiver
    var currentAfterParamsPC = currentPC

    // Process each parameter and update the PC accordingly
    for (param <- params) {
      currentAfterParamsPC = ExprProcessor.processExpression(param, instructionsWithPCs, currentAfterParamsPC)
    }
    val instruction = INVOKESTATIC(declaringClass, isInterface, methodName, methodDescriptor)
    instructionsWithPCs += ((currentAfterParamsPC, instruction))
    currentAfterParamsPC + instruction.length
  }

  def processGoto(instructionsWithPCs: ArrayBuffer[(Int, Instruction)], currentPC: Int): Int = {
    val instruction = GOTO(-1)
    instructionsWithPCs += ((currentPC, instruction))
    val length = instruction.length
    currentPC + length
  }

  def processIf(left: Expr[_], condition: RelationalOperator, right: Expr[_], gotoLabel: Int, instructionsWithPCs: ArrayBuffer[(Int, Instruction)], currentPC: Int): Int = {
    // process the left expr and save the pc to give in the right expr processing
    val leftPC = ExprProcessor.processExpression(left, instructionsWithPCs, currentPC)
    // process the right expr
    val rightPC = ExprProcessor.processExpression(right, instructionsWithPCs, leftPC)
    generateIfInstruction(left, condition, right, instructionsWithPCs, currentPC, rightPC)
  }

  def generateIfInstruction(left: Expr[_], condition: RelationalOperator, right: Expr[_], instructionsWithPCs: ArrayBuffer[(Int, Instruction)], currentPC: Int, rightPC: Int): Int = {
    val instruction = (left.cTpe, right.cTpe) match {
      // Handle null comparisons
      case (_, _) if right.isNullExpr || left.isNullExpr =>
        condition match {
          case EQ  => IFNULL(-1)
          case NE  => IFNONNULL(-1)
          case _ => throw new UnsupportedOperationException(s"Unsupported condition: $condition")
        }

      // Handle reference comparisons (object references)
      case (ComputationalTypeReference, ComputationalTypeReference) =>
        condition match {
          case EQ => IF_ACMPEQ(-1)
          case NE => IF_ACMPNE(-1)
          case _ => throw new UnsupportedOperationException(s"Unsupported condition: $condition")
        }

      // Handle integer comparisons
      case (ComputationalTypeInt, ComputationalTypeInt) =>
        condition match {
          case EQ  => IF_ICMPEQ(-1)
          case NE  => IF_ICMPNE(-1)
          case LT  => IF_ICMPLT(-1)
          case LE  => IF_ICMPLE(-1)
          case GT  => IF_ICMPGT(-1)
          case GE  => IF_ICMPGE(-1)
          case _ => throw new UnsupportedOperationException(s"Unsupported condition: $condition")
        }

      // Handle unsupported types
      case _ => throw new UnsupportedOperationException(s"Unsupported types: left = ${left.cTpe}, right = ${right.cTpe}")
    }

    instructionsWithPCs += ((rightPC, instruction))
    rightPC + instruction.length
  }
}
