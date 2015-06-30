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
package cg

import scala.language.postfixOps
import java.net.URL
import org.opalj.fp.Result
import org.opalj.fp.Entity
import org.opalj.fp.PropertyStore
import org.opalj.fp.PropertyKey
import org.opalj.fp.Property
import org.opalj.fp.PropertyComputationResult
import org.opalj.fp.Result
import org.opalj.br.analyses.Project
import org.opalj.br.analyses.SourceElementsPropertyStoreKey
import org.opalj.br.ClassFile
import org.opalj.br.ObjectType
import org.opalj.br.analyses.SomeProject
import org.opalj.fp.ImmediateResult
import org.opalj.br.Method
import org.opalj.ai.AI
import org.opalj.br.instructions.AASTORE
import org.opalj.br.instructions.ATHROW
import org.opalj.br.instructions.INVOKEDYNAMIC
import org.opalj.br.instructions.PUTSTATIC
import org.opalj.br.instructions.PUTFIELD
import org.opalj.br.instructions.FieldWriteAccess
import org.opalj.br.instructions.INVOKEINTERFACE
import org.opalj.br.instructions.INVOKESPECIAL
import org.opalj.br.instructions.INVOKESTATIC
import org.opalj.br.instructions.INVOKEVIRTUAL
import org.opalj.br.instructions.MethodInvocationInstruction

/**
 * This property determines if an object leaks it's self reference (`this`) by passing
 *  it to methods or assigning it to fields.
 */
sealed trait SelfReferenceLeakage extends Property {
    final def key = SelfReferenceLeakage.Key
}

object SelfReferenceLeakage {
    final val Key = PropertyKey.create("SelfReferenceLeakage", LeaksSelfReference)
}

case object LeaksSelfReference extends SelfReferenceLeakage

case object DoesNotLeakSelfReference extends SelfReferenceLeakage

/**
 * A shallow analysis that determines for each object that is created within a method (new)
 * if it escapes the scope of the method.
 *
 * An object escapes the scope of a method if:
 *  - ... it is assigned to a field,
 *  - ... it is passed to a method,
 *  - ... it is stored in a field,
 *  - ... it is returned,
 *  - ... the object itself leaks it's self reference (`this`) by:
 *      - ... storing `this` in some static field or,
 *      - ... storing it's self reference in a data-structure (another object or array)
 *        passed to it (by assigning to a field or calling a method),
 *      - ... if a superclass leaks the self reference.
 *
 * This analysis can be used as a foundation for an analysis that determines whether
 * all instances created for a specific class never escape the creating method and,
 * hence, respective types cannot occur.
 */
object EscapeAnalysis {

    val SelfReferenceLeakage = org.opalj.cg.SelfReferenceLeakage.Key

    /**
     * Determines for the given class file if any method may leak the self reference (`this`).
     *
     * Hence, it only makes sense to call this method if all supertypes do not leak
     * their self reference.
     */
    private[this] def determineSelfReferenceLeakageContinuation(
        classFile: ClassFile)(
            implicit project: SomeProject, store: PropertyStore): PropertyComputationResult = {

        val classType = classFile.thisType
        val classHierarchy = project.classHierarchy

        def isSubtype(otherType: ObjectType): Boolean = {
            classHierarchy.isSubtypeOf(classType, otherType.asObjectType).isYesOrUnknown
        }

        // This method just implements a very quick check if there is any potential
        // that the method may leak it's self reference. Hence, if this method returns
        // true, a more thorough analysis is useful/necessary.
        def potentiallyLeaksSelfReference(method: Method): Boolean = {
            val returnType = method.returnType
            if (returnType.isObjectType && isSubtype(returnType.asObjectType))
                return true;
            val body = method.body.get
            val instructions = body.instructions
            val max = instructions.length
            var pc = 0
            while (pc < max) {
                val instruction = instructions(pc)
                instruction.opcode match {
                    case AASTORE.opcode ⇒
                        return true;
                    case ATHROW.opcode if isSubtype(ObjectType.Throwable) ⇒
                        // the exception throws itself...
                        return true;
                    case INVOKEDYNAMIC.opcode ⇒
                        return true;
                    case INVOKEINTERFACE.opcode |
                        INVOKESPECIAL.opcode |
                        INVOKESTATIC.opcode |
                        INVOKEVIRTUAL.opcode ⇒
                        val invoke = instruction.asInstanceOf[MethodInvocationInstruction]
                        val parameterTypes = invoke.methodDescriptor.parameterTypes
                        if (parameterTypes.exists { pt ⇒ pt.isObjectType && isSubtype(pt.asObjectType) })
                            return true;
                    case PUTSTATIC.opcode | PUTFIELD.opcode ⇒
                        val fieldType = instruction.asInstanceOf[FieldWriteAccess].fieldType
                        if (fieldType.isObjectType && isSubtype(fieldType.asObjectType))
                            return true;
                    case _ ⇒ /*nothing to do*/
                }
                pc = instruction.indexOfNextInstruction(pc, body)
            }

            return false;
        }

        def leaksSelfReference(method: Method): Boolean = {
            //    AI()
            return true;
        }

        val doesLeakSelfReference =
            classFile.methods.exists { m ⇒
                !m.isStatic &&
                    !m.isAbstract &&
                    (m.isNative || (potentiallyLeaksSelfReference(m) && leaksSelfReference(m)))
            }
        if (doesLeakSelfReference)
            ImmediateResult(classFile, LeaksSelfReference);
        else
            ImmediateResult(classFile, DoesNotLeakSelfReference);
    }

    def determineSelfReferenceLeakage(
        classFile: ClassFile)(
            implicit project: SomeProject, store: PropertyStore): PropertyComputationResult = {

        if (classFile.thisType eq ObjectType.Object)
            return ImmediateResult(classFile, DoesNotLeakSelfReference);

        // let's check the supertypes w.r.t. their leakage property
        val superclassFileOption = project.classFile(classFile.superclassType.get)
        val interfaceTypesOption = classFile.interfaceTypes.map(project.classFile(_))
        if (superclassFileOption.isEmpty || interfaceTypesOption.exists(_ == None))
            // The project is not complete, hence, we have to use the fallback.
            return ImmediateResult(classFile, LeaksSelfReference);

        val superClassFiles = superclassFileOption.get +: interfaceTypesOption.map(_.get)

        store.allHaveProperty(
            /*depender*/ classFile, SelfReferenceLeakage,
            /*dependees*/ superClassFiles, DoesNotLeakSelfReference) { haveProperty ⇒
                if (haveProperty)
                    determineSelfReferenceLeakageContinuation(classFile)
                else
                    ImmediateResult(classFile, LeaksSelfReference);
            }
    }

    def analyze(implicit project: Project[URL]): Unit = {
        implicit val store = project.get(SourceElementsPropertyStoreKey)
        val filter: PartialFunction[Entity, ClassFile] = { case cf: ClassFile ⇒ cf }
        store <||< (filter, determineSelfReferenceLeakage)
    }
}
