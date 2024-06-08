/* BSD 2-Clause License - see OPAL/LICENSE for details. */
package org.opalj.tac.tactobc

import org.opalj.RelationalOperator
import org.opalj.RelationalOperators._
import org.opalj.br.{ComputationalTypeDouble, ComputationalTypeFloat, ComputationalTypeInt, ComputationalTypeLong, MethodDescriptor, ReferenceType}
import org.opalj.br.instructions.{ARETURN, FRETURN, GOTO, IFNONNULL, IFNULL, IF_ICMPEQ, IF_ICMPGE, IF_ICMPGT, IF_ICMPLE, IF_ICMPLT, IF_ICMPNE, INVOKEVIRTUAL, IRETURN, Instruction, LRETURN, RETURN}
import org.opalj.collection.immutable.IntIntPair
import org.opalj.tac.{Expr, Var}

import scala.collection.immutable.ArraySeq
import scala.collection.mutable.ArrayBuffer

object StmtProcessor {

  //Assignment
  def processAssignment(targetVar: Var[_], expr: Expr[_], instructionsWithPCs: ArrayBuffer[(Int, Instruction)], currentPC: Int): Int = {
    // Evaluate the RHS and update the PC accordingly
    val afterExprPC = ExprUtils.processExpression(expr, instructionsWithPCs, currentPC)
    // Store the result into the target variable and update the PC
    val finalPC = ExprUtils.storeVariable(targetVar, instructionsWithPCs, afterExprPC)
    // Return the updated PC
    finalPC
  }

  /*def processSwitch(defaultTarget: Int, index: Expr[_], npairs: ArraySeq[IntIntPair /*(Case Value, Jump Target)*/]): Int = {
    val bCnpairs = ArraySeq[IntIntPair](npairs.size)
    npairs.foreach { (, _2)
      bCnpairs += ()
    }
    1
  }*/

  def processReturn(instructionsWithPCs: ArrayBuffer[(Int, Instruction)], currentPC: Int): Int = {
    val instruction = RETURN
    instructionsWithPCs += ((currentPC, instruction))
    currentPC + instruction.length
  }

  def processReturnValue(expr: Expr[_], instructionsWithPCs: ArrayBuffer[(Int, Instruction)], currentPC: Int): Int = {
    val afterExprPC = ExprUtils.processExpression(expr, instructionsWithPCs, currentPC)
    val instruction = expr.cTpe match {
      case ComputationalTypeInt => IRETURN
      case ComputationalTypeLong => LRETURN
      case ComputationalTypeFloat => FRETURN
      case ComputationalTypeDouble => ARETURN
      case _ => throw new UnsupportedOperationException("Unsupported computational type:" + expr.cTpe)
    }
    val offsetPC = currentPC + (afterExprPC - currentPC)
    instructionsWithPCs += ((currentPC, instruction))
    currentPC + offsetPC
  }

  def processVirtualMethodCall(declaringClass: ReferenceType, isInterface: Boolean, methodName: String, methodDescriptor: MethodDescriptor, receiver: Expr[_], params: Seq[Expr[_]], instructionsWithPCs: ArrayBuffer[(Int, Instruction)], currentPC: Int): Int = {
    val instruction = /*if (isInterface) {
      INVOKEINTERFACE
    }else*/
      INVOKEVIRTUAL(declaringClass, methodName, methodDescriptor)
    instructionsWithPCs += ((currentPC, instruction))
    currentPC + instruction.length
  }

  def processGoto(instructionsWithPCs: ArrayBuffer[(Int, Instruction)], currentPC: Int): Int = {
    val instruction = GOTO(-1)
    instructionsWithPCs += ((currentPC, instruction))
    val length = instruction.length
    currentPC + length
  }

  def processIf(left: Expr[_], condition: RelationalOperator, right: Expr[_], gotoLabel: Int, instructionsWithPCs: ArrayBuffer[(Int, Instruction)], currentPC: Int): Int = {
    // process the left expr and save the pc to give in the right expr processing
    val leftPC = ExprUtils.processExpression(left, instructionsWithPCs, currentPC)
    // process the right expr
    val rightPC = ExprUtils.processExpression(right, instructionsWithPCs, leftPC)
    generateIfInstruction(left, condition, right, instructionsWithPCs, currentPC, rightPC)
  }

  def generateIfInstruction(left: Expr[_], condition: RelationalOperator, right: Expr[_], instructionsWithPCs: ArrayBuffer[(Int, Instruction)], currentPC: Int, rightPC: Int): Int = {
    val instruction = (left, right) match {
      case _ if right.isNullExpr || left.isNullExpr =>
        condition match {
          case EQ  => IFNULL(-1)
          case NE  => IFNONNULL(-1)
          case _ => throw new UnsupportedOperationException(s"Unsupported condition: $condition")
        }
      case _ =>
        condition match {
          case EQ  => IF_ICMPEQ(-1)
          case NE  => IF_ICMPNE(-1)
          case LT  => IF_ICMPLT(-1)
          case LE  => IF_ICMPLE(-1)
          case GT  => IF_ICMPGT(-1)
          case GE  => IF_ICMPGE(-1)
          case _ => throw new UnsupportedOperationException(s"Unsupported condition: $condition")
        }
          /*case EQ => IF_ACMPEQ(-1)
          case NE  => IF_ACMPNE(-1)
          //Unsupported
          case _ => IFNE(-1)//throw new UnsupportedOperationException(s"Unsupported condition: $condition")*/
    }
    val offsetPC = {
      if(rightPC > 0 && rightPC > currentPC){
        currentPC + (rightPC - currentPC)
      } else {
        currentPC
      }
    }
    instructionsWithPCs += ((offsetPC, instruction))
    offsetPC + instruction.length
  }
}
