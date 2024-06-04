/* BSD 2-Clause License - see OPAL/LICENSE for details. */
package org.opalj.tac.tactobc

import org.opalj.RelationalOperator
import org.opalj.RelationalOperators.{EQ, GE, GT, LE, LT, NE}
import org.opalj.br.instructions.{IFEQ, IFGE, IFGT, IFLE, IFLT, IFNE, IFNONNULL, IFNULL, IF_ACMPEQ, IF_ACMPNE, IF_ICMPEQ, IF_ICMPGE, IF_ICMPGT, IF_ICMPLE, IF_ICMPLT, IF_ICMPNE, Instruction}
import org.opalj.tac.{DUVar, Expr, If, IntConst, Stmt, UVar, Var}

import scala.collection.mutable.ArrayBuffer

object StmtProcessor {

  //Assignment
  def processAssignment(targetVar: Var[_], expr: Expr[_], instructionsWithPCs: ArrayBuffer[(Int, Instruction)], currentPC: Int, s: Stmt[DUVar[_]], nextStmt: Stmt[DUVar[_]], loopHead: Boolean): Int = {
    val result = nextStmt match {
      //process for loops
      case If(_, left, condition, right, target) =>
        processForLoop(s, nextStmt, instructionsWithPCs, currentPC)
      //is a normal if
      case _ =>
        //Processing RHS
        val afterExprPC = ExprUtils.processExpression(expr, instructionsWithPCs, currentPC, isForLoop = false)
        //Processing LHS
        val finalPC = ExprUtils.storeVariable(targetVar, instructionsWithPCs, afterExprPC)
        finalPC
    }
    result
  }

  private def processForLoop(assignmentStmt: Stmt[DUVar[_]], ifStmt: Stmt[DUVar[_]], instructionsWithPCs: ArrayBuffer[(Int, Instruction)], currentPC: Int): Int = {
    val ifStmtValues = ifStmt.asIf
    processIf(ifStmtValues.left, ifStmtValues.condition, ifStmtValues.right, ifStmtValues.target, instructionsWithPCs, currentPC, ifStmt)
    val assignmentValues= assignmentStmt.asAssignment
    ExprUtils.processExpression(assignmentValues.expr, instructionsWithPCs, currentPC, isForLoop = false)
  }

  def processIf(left: Expr[_], condition: RelationalOperator, right: Expr[_], gotoLabel: Int, instructionsWithPCs: ArrayBuffer[(Int, Instruction)], currentPC: Int, previousStmt: Stmt[DUVar[_]]): Int = {
    //check if previous stmt was assignment
    /*if(previousStmt.isAssignment){
      //processForLoop already handles it
      return currentPC
    }*/
    // check if for loop
    val isForLoop = detectForLoop(left, right)
    // process the left expr and save the pc to give in the right expr processing
    val leftPC = ExprUtils.processExpression(left, instructionsWithPCs, currentPC, isForLoop)
    // process the right expr
    val rightPC = ExprUtils.processExpression(right, instructionsWithPCs, leftPC, isForLoop)
    generateIfInstruction(left, right, condition, instructionsWithPCs, currentPC, rightPC)
  }

  private def detectForLoop(left: Expr[_], right: Expr[_]): Boolean = (left, right) match {
    case (uVarLeft: UVar[_], uVarRight: UVar[_]) => uVarLeft.defSites.nonEmpty && uVarRight.defSites.nonEmpty
    case _ => false
  }

  def generateIfInstruction(left: Expr[_], right: Expr[_], condition: RelationalOperator, instructionsWithPCs: ArrayBuffer[(Int, Instruction)], currentPC: Int, rightPC: Int): Int = {
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
    val offsetPC = {
      if(rightPC > 0 && rightPC > currentPC){
        currentPC + (rightPC - currentPC)
      }else{
        return currentPC
      }
    }
    instructionsWithPCs += ((offsetPC, instruction))
    offsetPC + instruction.length
  }
}
