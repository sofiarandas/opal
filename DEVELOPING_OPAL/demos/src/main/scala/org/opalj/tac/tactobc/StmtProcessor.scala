package org.opalj.tac.tactobc

import org.opalj.RelationalOperator
import org.opalj.br.instructions.Instruction
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
    1
  }

}
