/* BSD 2-Clause License - see OPAL/LICENSE for details. */
package org.opalj.tac.tactobc

import org.opalj.br.instructions.Instruction
import org.opalj.tac.{Expr, Var}

import scala.collection.mutable.ArrayBuffer

object AssignmentUtils {

  def processAssignment(targetVar: Var[_], expr: Expr[_], instructionsWithPCs: ArrayBuffer[(Int, Instruction)], currentPC: Int): Int = {
    // Evaluate the RHS and update the PC accordingly
    val afterExprPC = ExprUtils.processExpression(expr, instructionsWithPCs, currentPC)

    // Store the result into the target variable and update the PC
    val finalPC = ExprUtils.storeVariable(targetVar, instructionsWithPCs, afterExprPC)

    // Return the updated PC
    finalPC
  }
}
