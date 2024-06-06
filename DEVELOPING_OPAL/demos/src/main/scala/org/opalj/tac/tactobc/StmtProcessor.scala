/* BSD 2-Clause License - see OPAL/LICENSE for details. */
package org.opalj.tac.tactobc

import org.opalj.RelationalOperator
import org.opalj.RelationalOperators._
import org.opalj.br.instructions.{GOTO, IFEQ, IFGE, IFGT, IFLE, IFLT, IFNE, IFNONNULL, IFNULL, IF_ICMPEQ, IF_ICMPGE, IF_ICMPGT, IF_ICMPLE, IF_ICMPLT, IF_ICMPNE, Instruction}
import org.opalj.tac.{DUVar, Expr, IntConst, Stmt, Var}

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

  def processGoto(assignmentStmt: Stmt[DUVar[_]], instructionsWithPCs: ArrayBuffer[(Int, Instruction)], currentPC: Int): Int = {
    val assignmentValues= assignmentStmt.asAssignment
    val afterIincPC = ExprUtils.handleBinaryExpr(assignmentValues.expr.asBinaryExpr, instructionsWithPCs, currentPC)
    val instruction = GOTO(-1)
    instructionsWithPCs += ((afterIincPC, instruction))
    val length = instruction.length
    afterIincPC + length
  }

 /* private def processForLoop(assignmentStmt: Stmt[DUVar[_]], ifStmt: Stmt[DUVar[_]], instructionsWithPCs: ArrayBuffer[(Int, Instruction)], currentPC: Int): Int = {
    println(s"Processing for loop with assignment: $assignmentStmt and if: $ifStmt")
    // >(1) load needed variable for comparison
    val ifStmtValues = ifStmt.asIf
    val afterLoadingVarPC = ExprUtils.loadVariable(ifStmtValues.left.asVar, instructionsWithPCs, currentPC)
    // >(2) handle Loop Anker for comparison
    val assignmentValues= assignmentStmt.asAssignment
    val afterLoopAnkerPC = ExprUtils.processExpression(assignmentValues.expr, instructionsWithPCs, afterLoadingVarPC)
    // >(3) generate IF instruction
    generateIfInstruction(ifStmtValues.left, ifStmtValues.condition, ifStmtValues.right, instructionsWithPCs, afterLoopAnkerPC, 0)
  }*/

  def processIf(left: Expr[_], condition: RelationalOperator, right: Expr[_], gotoLabel: Int, instructionsWithPCs: ArrayBuffer[(Int, Instruction)], currentPC: Int, previousStmt: Stmt[DUVar[_]]): Int = {
    //check if previous stmt was assignment
    if(previousStmt.isAssignment){
      //processForLoop already handles it, skip stmt processing but save target (?)
      return currentPC
    }
    // process the left expr and save the pc to give in the right expr processing
    val leftPC = ExprUtils.processExpression(left, instructionsWithPCs, currentPC)
    if(right.asIntConst.value == 0){
      //comparing with 0 -> no need to load constant
      return generateIfInstruction(left, condition, right, instructionsWithPCs, leftPC, 0)
    }
    // process the right expr
    val rightPC = ExprUtils.processExpression(right, instructionsWithPCs, leftPC)
    generateIfInstruction(left, condition, right, instructionsWithPCs, currentPC, rightPC)
  }

 /* private def detectForLoop(left: Expr[_], right: Expr[_]): Boolean = (left, right) match {
    case (uVarLeft: UVar[_], uVarRight: UVar[_]) => uVarLeft.defSites.nonEmpty && uVarRight.defSites.nonEmpty
    case _ => false
  }*/

  def generateIfInstruction(left: Expr[_], condition: RelationalOperator, right: Expr[_], instructionsWithPCs: ArrayBuffer[(Int, Instruction)], currentPC: Int, rightPC: Int): Int = {
    val instruction = (left, right) match {
      case (IntConst(_, 0), _) | (_, IntConst(_, 0)) =>
        condition match {
          //
          // Operators to compare int values.
          //
          case LT  => IFLT(-1)
          case GT  => IFGT(-1)
          case LE  => IFLE(-1)
          case GE  => IFGE(-1)
          //
          // Operators to compare int and reference values.
          //
          case EQ  => IFEQ(-1)
          case NE  => IFNE(-1)
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
          case _ => IFNE(-1)//throw new UnsupportedOperationException(s"Unsupported condition: $condition")
        }
      case _ if right.isNullExpr || left.isNullExpr =>
        condition match {
          case EQ  => IFNULL(-1)
          case NE  => IFNONNULL(-1)
          case _ => IFNE(-1)//throw new UnsupportedOperationException(s"Unsupported condition: $condition")
        }
      case _ =>
        condition match {
          case EQ  => IF_ICMPEQ(-1)
          case NE  => IF_ICMPNE(-1)
          case LT  => IF_ICMPLT(-1)
          case LE  => IF_ICMPLE(-1)
          case GT  => IF_ICMPGT(-1)
          case GE  => println(s"instruction ${IF_ICMPGE(-1)}")
                      IF_ICMPGE(-1)
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
