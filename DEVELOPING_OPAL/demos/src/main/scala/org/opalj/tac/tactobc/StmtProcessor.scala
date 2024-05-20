/* BSD 2-Clause License - see OPAL/LICENSE for details. */
package org.opalj.tac.tactobc

import org.opalj.RelationalOperator
import org.opalj.RelationalOperators.{CMP, CMPG, CMPL, EQ, GE, GT, LE, LT, NE}
import org.opalj.br.instructions.{IFEQ, IFGE, IFGT, IFLE, IFLT, IFNE, IFNONNULL, IFNULL, IF_ACMPEQ, IF_ACMPNE, IF_ICMPEQ, IF_ICMPGE, IF_ICMPGT, IF_ICMPLE, IF_ICMPLT, IF_ICMPNE, Instruction}
import org.opalj.tac.{Expr, Var}

import scala.collection.mutable.ArrayBuffer

object StmtProcessor {

  //Assignment
  def processAssignment(targetVar: Var[_], expr: Expr[_], instructionsWithPCs: ArrayBuffer[(Int, Instruction)], currentPC: Int): Int = {
    //Processing RHS
    val afterExprPC = ExprUtils.processExpression(expr, instructionsWithPCs, currentPC)
    //Processing LHS
    val finalPC = ExprUtils.storeVariable(targetVar, instructionsWithPCs, afterExprPC)
    finalPC
  }

  def processIf(left: Expr[_], condition: RelationalOperator, right: Expr[_], gotoLabel: Int, instructionsWithPCs: ArrayBuffer[(Int, Instruction)], currentPC: Int): Int = {
    // process the left expr and save the pc to give in the right expr processing
    val leftPC = ExprUtils.processExpression(left, instructionsWithPCs, currentPC)
    // process the right Expr
    val rightPC = ExprUtils.processExpression(right, instructionsWithPCs, leftPC)
    val instruction = {
      if (((left.isIntConst && left.asIntConst.value == 0) && (right.isIntConst && right.asIntConst.value != 0)
        || ((left.isIntConst && left.asIntConst.value != 0) && (right.isIntConst && right.asIntConst.value == 0)))) {
        condition match {
          //
          // Operators to compare int values.
          //
          case LT => IFLT(-1)
          case GT => IFGT(-1)
          case LE => IFLE(-1)
          case GE => IFGE(-1)
          //
          // Operators to compare int and reference values.
          //
          case EQ => IFEQ(-1)
          case NE => IFNE(-1)
          //
          // Operators to compare floating point numbers.
          //
          //ToDo: find out the difference
          /*case CMPG =>
          case CMPL =>
          //
          // Operators to compare long values.
          //
          case CMP =>
           */
        }
      } else if (left.isIntConst && right.isIntConst) {
        condition match {
          case EQ => IF_ICMPEQ(-1)
          case NE => IF_ICMPNE(-1)
          case LT => IF_ICMPLT(-1)
          case LE => IF_ICMPLE(-1)
          case GT => IF_ICMPGT(-1)
          case GE => IF_ICMPGE(-1)
        }
      } else if(right.isNullExpr || left.isNullExpr){
        condition match {
          case EQ => IFNULL(-1)
          case NE => IFNONNULL(-1)
        }
      } else {
        condition match {
          case EQ => IF_ACMPEQ(-1)
          case NE => IF_ACMPNE(-1)
        }
      }
    }
    val offsetPC = currentPC + (rightPC - currentPC)
    instructionsWithPCs += ((offsetPC, instruction))
    offsetPC + instruction.length
  }
}
