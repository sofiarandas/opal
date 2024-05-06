/* BSD 2-Clause License - see OPAL/LICENSE for details. */
package org.opalj.tac.tactobc

import org.opalj.br.instructions.Instruction
import org.opalj.tac.{Expr, Var}

import scala.collection.mutable.ArrayBuffer

object AssignmentUtils {

  def processAssignment(targetVar: Var[_], expr: Expr[_], instructionsWithPCs: ArrayBuffer[(Int, Instruction)], currentPC: Int): Int = {
    //RHS
    ExprUtils.processExpression(expr, instructionsWithPCs, currentPC)
    //LHS
    //ToDo: create some logic to handle the targetVar
    ExprUtils.processExpression(targetVar, instructionsWithPCs, currentPC)
    ExprUtils.storeVariable(targetVar, instructionsWithPCs, currentPC)
  }
}
