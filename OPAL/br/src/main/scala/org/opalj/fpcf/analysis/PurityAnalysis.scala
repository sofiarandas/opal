/* BSD 2-Clause License:
 * Copyright (c) 2009 - 2015
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
package org.opalj
package fpcf
package analysis

import org.opalj.br.PC
import org.opalj.br.Method
import org.opalj.br.analyses.SomeProject
import org.opalj.br.instructions.GETFIELD
import org.opalj.br.instructions.GETSTATIC
import org.opalj.br.instructions.PUTFIELD
import org.opalj.br.instructions.PUTSTATIC
import org.opalj.br.instructions.MONITORENTER
import org.opalj.br.instructions.MONITOREXIT
import org.opalj.br.instructions.NEW
import org.opalj.br.instructions.NEWARRAY
import org.opalj.br.instructions.MULTIANEWARRAY
import org.opalj.br.instructions.AALOAD
import org.opalj.br.instructions.AASTORE
import org.opalj.br.instructions.ARRAYLENGTH
import org.opalj.br.instructions.LALOAD
import org.opalj.br.instructions.IALOAD
import org.opalj.br.instructions.CALOAD
import org.opalj.br.instructions.BALOAD
import org.opalj.br.instructions.BASTORE
import org.opalj.br.instructions.CASTORE
import org.opalj.br.instructions.IASTORE
import org.opalj.br.instructions.LASTORE
import org.opalj.br.instructions.SASTORE
import org.opalj.br.instructions.SALOAD
import org.opalj.br.instructions.DALOAD
import org.opalj.br.instructions.FALOAD
import org.opalj.br.instructions.FASTORE
import org.opalj.br.instructions.DASTORE
import org.opalj.br.instructions.INVOKEDYNAMIC
import org.opalj.br.instructions.INVOKESTATIC
import org.opalj.br.instructions.INVOKESPECIAL
import org.opalj.br.instructions.INVOKEVIRTUAL
import org.opalj.br.instructions.INVOKEINTERFACE
import org.opalj.br.instructions.MethodInvocationInstruction
import org.opalj.fpcf.EOptionP
import org.opalj.fpcf.EP
import org.opalj.fpcf.Property
import org.opalj.fpcf.PropertyComputationResult
import org.opalj.fpcf.PropertyKind
import org.opalj.fpcf.PropertyStore
import org.opalj.fpcf.UpdateType
import org.opalj.fpcf.properties.Purity
import org.opalj.fpcf.properties.EffectivelyFinalField
import org.opalj.fpcf.properties.Impure
import org.opalj.fpcf.properties.FieldMutability
import org.opalj.fpcf.properties.MaybePure
import org.opalj.fpcf.properties.ConditionallyPure
import org.opalj.br.instructions.ANEWARRAY
import org.opalj.fpcf.properties.Pure
import org.opalj.fpcf.properties.ImmutableType
import org.opalj.fpcf.properties.TypeImmutability

/**
 * This analysis determines whether a method is pure. I.e., whether the method
 * only operates on the given state (i.e., the method is pure) or
 * depends on other state/mutable global state; the given state may include the state of the
 * current object that is the receiver of the call if the object/receiver is immutable.
 *
 * '''This analysis follows the definition found on wikipedia:'''
 *
 * [...] a function may be considered a pure function if both of the following statements about
 * the function hold:
 *  - 	The function always evaluates to the same result value given the same argument value(s).
 *  	The function result value cannot depend on any hidden information or state that may change
 *  	while program execution proceeds or between different executions of the program, nor can it
 *  	depend on any external input from I/O devices.
 *
 *  	'''[Hence, using true constants (e.g., Math.e) is not a problem as well as creating
 *  	intermediate
 *  	(mutable) data structures. Furthermore, instance method based calls can also be pure if
 *  	the receiving object is (effectively final).'''
 *
 *  -	Evaluation of the result does not cause any semantically observable side effect or output,
 *  	such as mutation of mutable objects or output to I/O devices.
 *  	The result value need not depend on all (or any) of the argument values. However, it must
 *  	depend on nothing other than the argument values. The function may return multiple result
 *  	values and these conditions must apply to all returned values for the function to be
 *  	considered pure. If an argument is call by reference, any parameter mutation will alter
 *  	the value of the argument outside the function, which will render the function impure.
 *   	'''However, if the referenced object is immutable it is object.'''
 *
 * @author Michael Eichberg
 */
class PurityAnalysis private (val project: SomeProject) extends FPCFAnalysis {

    /**
     * Determines the purity of the method starting with the instruction with the given
     * pc. If the given pc is larger than 0 then all previous instructions (in particular
     * method calls) must not violate this method's purity.
     *
     * This function encapsulates the continuation.
     */
    private[this] def doDeterminePurity(
        method:           Method,
        pc:               PC,
        initialDependees: Set[EOptionP[Method, Purity]]
    ): PropertyComputationResult = {

        val declaringClassType = project.classFile(method).thisType
        val methodDescriptor = method.descriptor
        val methodName = method.name
        val body = method.body.get
        val instructions = body.instructions
        val maxPC = instructions.size

        var dependees = initialDependees

        var currentPC = pc
        while (currentPC < maxPC) {
            val instruction = instructions(currentPC)

            (instruction.opcode: @scala.annotation.switch) match {
                case GETSTATIC.opcode ⇒
                    val GETSTATIC(declaringClass, fieldName, fieldType) = instruction
                    import project.classHierarchy.resolveFieldReference
                    resolveFieldReference(declaringClass, fieldName, fieldType, project) match {

                        case Some(field) if field.isFinal ⇒
                        /* Nothing to do; constants do not impede purity! */

                        case Some(field) if field.isPrivate /*&& field.isNonFinal*/ ⇒
                            // We are suspending this computation and wait for the result.
                            return propertyStore.require(
                                method, Purity.key,
                                field, FieldMutability.key
                            ) { (e: Entity, dependeeP: Property) ⇒
                                if (dependeeP == EffectivelyFinalField) {
                                    val nextPC = body.pcOfNextInstruction(currentPC)
                                    doDeterminePurity(method, nextPC, dependees)
                                } else {
                                    Result(method, Impure)
                                }
                            };

                        case _ ⇒
                            // We know nothing about the target field (it is not
                            // found in the scope of the current project).
                            return ImmediateResult(method, Impure);
                    }

                case INVOKESPECIAL.opcode | INVOKESTATIC.opcode ⇒ instruction match {

                    case MethodInvocationInstruction(`declaringClassType`, `methodName`, `methodDescriptor`) ⇒
                    // We have a self-recursive call; such calls do not influence
                    // the computation of the method's purity and are ignored.
                    // Let's continue with the evaluation of the next instruction.

                    case MethodInvocationInstruction(declaringClassType, methodName, methodDescriptor) ⇒
                        import project.classHierarchy.lookupMethodDefinition
                        val calleeOption =
                            lookupMethodDefinition(
                                declaringClassType.asObjectType /* this is safe...*/ ,
                                methodName,
                                methodDescriptor,
                                project
                            )
                        calleeOption match {
                            case None ⇒
                                // We know nothing about the target method (it is not
                                // found in the scope of the current project).
                                return ImmediateResult(method, Impure);

                            case Some(callee) ⇒
                                /* Recall that self-recursive calls are handled earlier! */
                                val purity = propertyStore(callee, Purity.key)

                                purity match {
                                    case EP(_, Pure) ⇒ /* Nothing to do...*/

                                    case EP(_, Impure | MaybePure) ⇒
                                        return ImmediateResult(method, Impure);

                                    // Handling cyclic computations
                                    case ep @ EP(_, ConditionallyPure) ⇒
                                        dependees += ep

                                    case epk ⇒
                                        dependees += epk
                                }
                        }
                }

                case NEW.opcode |
                    GETFIELD.opcode |
                    PUTFIELD.opcode | PUTSTATIC.opcode |
                    NEWARRAY.opcode | MULTIANEWARRAY.opcode | ANEWARRAY.opcode |
                    AALOAD.opcode | AASTORE.opcode |
                    BALOAD.opcode | BASTORE.opcode |
                    CALOAD.opcode | CASTORE.opcode |
                    SALOAD.opcode | SASTORE.opcode |
                    IALOAD.opcode | IASTORE.opcode |
                    LALOAD.opcode | LASTORE.opcode |
                    DALOAD.opcode | DASTORE.opcode |
                    FALOAD.opcode | FASTORE.opcode |
                    ARRAYLENGTH.opcode |
                    MONITORENTER.opcode | MONITOREXIT.opcode |
                    INVOKEDYNAMIC.opcode | INVOKEVIRTUAL.opcode | INVOKEINTERFACE.opcode ⇒
                    // improve: If the data-structure was created locally... then we don't care.
                    return ImmediateResult(method, Impure);

                case _ ⇒
                /* All other instructions (IFs, Load/Stores, Arith., etc.) are pure. */
            }
            currentPC = body.pcOfNextInstruction(currentPC)
        }

        // Every method that is not identified as being impure is (conditionally)pure.
        if (dependees.isEmpty)
            return ImmediateResult(method, Pure);

        def c(e: Entity, p: Property, u: UpdateType): PropertyComputationResult = {
            p match {
                case Impure | MaybePure ⇒
                    Result(method, Impure)

                case ConditionallyPure ⇒
                    dependees = dependees.filter { _.e ne e }
                    val newDependees = dependees + EP(e.asInstanceOf[Method], p.asInstanceOf[Purity])
                    IntermediateResult(method, ConditionallyPure, newDependees, c)

                case Pure ⇒
                    dependees = dependees.filter { _.e ne e }
                    if (dependees.isEmpty)
                        Result(method, Pure)
                    else
                        IntermediateResult(method, ConditionallyPure, dependees, c)

            }
        }

        IntermediateResult(method, ConditionallyPure, dependees, c)

    }

    /**
     * Determines the purity of the given method.
     */
    def determinePurity(method: Method): PropertyComputationResult = {

        // Due to a lack of knowledge, we classify all native methods or methods that
        // have no body because they are loaded using a library class file loader as Impure.
        if (method.body.isEmpty /*HERE: method.isNative || "isLibraryMethod(method)"*/ )
            return ImmediateResult(method, Impure);

        // All parameters either have to be base types or have to be immutable.
        val referenceTypeParameters = method.parameterTypes.filterNot(_.isBaseType)
        if (!referenceTypeParameters.forall { p ⇒ propertyStore.isKnown(p) })
            return ImmediateResult(method, Impure);

        propertyStore.allHaveProperty(
            method, Purity.key, referenceTypeParameters, ImmutableType
        ) { areImmutable ⇒
            if (areImmutable) {
                doDeterminePurity(method, 0, Set.empty[EOptionP[Method, Purity]])
            } else {
                ImmediateResult(method, Impure)
            }
        }
    }
}

/**
 *
 */
object PurityAnalysis extends FPCFAnalysisRunner {

    final val entitySelector: PartialFunction[Entity, Method] = {
        FPCFAnalysisRunner.NonAbstractMethodSelector
    }

    override def recommendations: Set[FPCFAnalysisRunner] = Set.empty

    override def derivedProperties: Set[PropertyKind] = Set(Purity)

    override def usedProperties: Set[PropertyKind] = Set(TypeImmutability, FieldMutability)

    def start(project: SomeProject, propertyStore: PropertyStore): FPCFAnalysis = {
        val analysis = new PurityAnalysis(project)
        propertyStore <||< (entitySelector, analysis.determinePurity)
        analysis
    }
}