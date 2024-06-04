/* BSD 2-Clause License - see OPAL/LICENSE for details. */
package org.opalj.tac.tactobc

import org.opalj.RelationalOperator
import org.opalj.RelationalOperators.{EQ, GE, GT, LE, LT, NE}
import org.opalj.br.instructions.{IFEQ, IFGE, IFGT, IFLE, IFLT, IFNE, IFNONNULL, IFNULL, IF_ACMPEQ, IF_ACMPNE, IF_ICMPEQ, IF_ICMPGE, IF_ICMPGT, IF_ICMPLE, IF_ICMPLT, IF_ICMPNE, Instruction}
import org.opalj.tac.{Const, DVar, Expr, IntConst, Stmt, UVar, Var}

import scala.collection.mutable.ArrayBuffer

object StmtProcessor {

  //Assignment
  def processAssignment(s: Stmt[_],targetVar: Var[_], expr: Expr[_], instructionsWithPCs: ArrayBuffer[(Int, Instruction)], currentPC: Int): Int = {
    if(targetVar.asInstanceOf[DVar[_]].useSites.size == 1){
      //Only loading
      val afterExprPC = ExprUtils.processExpression(expr, instructionsWithPCs, currentPC, isForLoop = false)
      return afterExprPC
    }
    //Processing RHS
    val afterExprPC = ExprUtils.processExpression(expr, instructionsWithPCs, currentPC, isForLoop = false)
    //Processing LHS
    val finalPC = ExprUtils.storeVariable(targetVar, instructionsWithPCs, afterExprPC)
    finalPC
  }

  def processIf(left: Expr[_], condition: RelationalOperator, right: Expr[_], gotoLabel: Int, instructionsWithPCs: ArrayBuffer[(Int, Instruction)], currentPC: Int): Int = {
    // check if for loop
    val isForLoop = detectForLoop(left, right)
    if (isForLoop) {
      // Handle for loop

    }
    // process the right expr and save the pc to give in the left expr processing
    val rightPC = ExprUtils.processExpression(right, instructionsWithPCs, currentPC, isForLoop)
    // process the left expr
    val leftPC = ExprUtils.processExpression(left, instructionsWithPCs, rightPC, isForLoop)
    val instruction = (left, right) match {
      case (IntConst(_, 0), _) | (_, IntConst(_, 0)) =>
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
      case (IntConst(_, _), IntConst(_, _)) =>
        condition match {
          case EQ => IF_ICMPEQ(-1)
          case NE => IF_ICMPNE(-1)
          case LT => IF_ICMPLT(-1)
          case LE => IF_ICMPLE(-1)
          case GT => IF_ICMPGT(-1)
          case GE => IF_ICMPGE(-1)
        }
      case _ if right.isNullExpr || left.isNullExpr =>
        condition match {
          case EQ => IFNULL(-1)
          case NE => IFNONNULL(-1)
        }
      case _ =>
        condition match {
          case EQ => IF_ACMPEQ(-1)
          case NE => IF_ACMPNE(-1)
          //Unsupported
          case _ =>
            println("this is fake: ")
            IFGE(-1)
        }
      }
    val offsetPC = currentPC + (leftPC - currentPC)
    instructionsWithPCs += ((offsetPC, instruction))
    offsetPC + instruction.length
  }


  private def detectForLoop(left: Expr[_], right: Expr[_]): Boolean = left match {
    case uVar: UVar[_] if uVar.defSites.nonEmpty =>
      right match {
        case _: UVar[_] | _: Const => true
        case _ => false
      }
    case _ => false
  }
}
