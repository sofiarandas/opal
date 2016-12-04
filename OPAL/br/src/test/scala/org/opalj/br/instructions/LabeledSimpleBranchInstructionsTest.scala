/* BSD 2-Clause License:
 * Copyright (c) 2009 - 2016
 * Software Technology Group
 * Department of Computer Science
 * Technische Universität Darmstadt
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *  - Redistributions of source code must retain the above copyright notice,
 *    this list of conditions and the following disclaimer.
 *  - Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */
package org.opalj.br.instructions

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FlatSpec, Matchers}

/**
 * Tests instantiation and resolving of LabeledInstructions.
 *
 * @author Malte Limmeroth
 */
@RunWith(classOf[JUnitRunner])
class LabeledSimpleBranchInstructionsTest extends FlatSpec with Matchers {
    behavior of "LabeledSimpleBranchInstructions"

    val label = 'TestLabel
    val simpleBranchInstructionsMap: List[(LabeledSimpleConditionalBranchInstruction, Symbol ⇒ LabeledSimpleConditionalBranchInstruction)] = {
        List /*[(LabeledSimpleConditionalBranchInstruction, { def apply(branchTarget: Symbol): AnyRef })]*/ (
            IFEQ(label) → LabeledIFEQ,
            IFNE(label) → LabeledIFNE,
            IFLT(label) → LabeledIFLT,
            IFGE(label) → LabeledIFGE,
            IFGT(label) → LabeledIFGT,
            IFLE(label) → LabeledIFLE,

            IF_ICMPEQ(label) → LabeledIF_ICMPEQ,
            IF_ICMPNE(label) → LabeledIF_ICMPNE,
            IF_ICMPLT(label) → LabeledIF_ICMPLT,
            IF_ICMPGE(label) → LabeledIF_ICMPGE,
            IF_ICMPGT(label) → LabeledIF_ICMPGT,
            IF_ICMPLE(label) → LabeledIF_ICMPLE,
            IF_ACMPEQ(label) → LabeledIF_ACMPEQ,
            IF_ACMPNE(label) → LabeledIF_ACMPNE,

            IFNULL(label) → LabeledIFNULL,
            IFNONNULL(label) → LabeledIFNONNULL
        )
    }

    val resolvedSimpleBranchInstructions = {
        simpleBranchInstructionsMap.zipWithIndex.map { instructionWithIndex ⇒
            val ((labeledInstruction, _), index) = instructionWithIndex
            labeledInstruction.resolveJumpTargets(0, Map(label → index))
        }
    }

    "the convenience factories of SimpleConditionalBranchInstructions" should
        "return the correct type of LabeledBranchInstruction" in {
            simpleBranchInstructionsMap foreach { bi ⇒
                val (factoryMethodResult, constructorResult) = bi
                assert(factoryMethodResult == constructorResult(label))
            }
        }

    "resolving SimpleBranchInstructions" should "resolve to the correct branchoffset" in {
        for ((i, index) ← resolvedSimpleBranchInstructions.zipWithIndex) {
            assert(i.branchoffset == index)
        }
    }

    "the convenience factories of GotoInstructions" should
        "return the correct type of LabeledGotoInstruction" in {
            assert(GOTO(label) == LabeledGOTO(label))
            assert(GOTO_W(label) == LabeledGOTO_W(label))
        }

    "resolving GotoInstructions" should "resolve to the correct branchoffset" in {
        assert(GOTO(label).resolveJumpTargets(1, Map(label → 43)).branchoffset == 42)
        assert(GOTO_W(label).resolveJumpTargets(2, Map(label → 44)).branchoffset == 42)
    }

}