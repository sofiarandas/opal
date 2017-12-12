/* BSD 2-Clause License:
 * Copyright (c) 2009 - 2017
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
package br
package analyses

import java.net.URL
import org.opalj.ai.analyses.{MethodReturnValuesAnalysis ⇒ TheAnalysis}

/**
 * Demonstrates how to extract generic type information.
 *
 * @author Michael Eichberg
 */
object GenericTypeInformationExtraction extends DefaultOneStepAnalysis {

    override def title: String = "demonstrates how to extract generic type information associated with a class"

    override def description: String = TheAnalysis.description

    override def doAnalyze(
        theProject:    Project[URL],
        parameters:    Seq[String],
        isInterrupted: () ⇒ Boolean
    ): BasicReport = {
        // THE NAME OF THE CONTAINER (HERE) HAS TO BE AN INTERFACE NAME!
        //        val containerPackageName = "java/lang/"
        //        val containerSimpleName = "Iterable"
        //        val containerSimpleName = "Comparable"

        //        val containerPackageName = "java/util/"
        //        val containerSimpleName = "Iterator"
        //    val containerSimpleName = "Collection"
        //    val containerSimpleName = "Comparator"
        //val containerSimpleName = "Enumeration"

        val containerPackageName = "java/util/concurrent/"
        val containerSimpleName = "Future"

        val containerName = containerPackageName + containerSimpleName
        val containerType = ObjectType(containerName)
        if (!theProject.classHierarchy.isKnown(containerType))
            return BasicReport(s"the type $containerType is unknown");

        val subtypes = theProject.classHierarchy.allSubtypes(containerType, false)

        val typeBindingSubtypes = for {
            subtype ← subtypes
            subtypeClassFile ← theProject.classFile(subtype).toSeq
            //ClassSignature(_, _, List(ClassTypeSignature(_, SimpleClassTypeSignature(_, _, t),_))) ← iteratorClassFile.classSignature

            ClassSignature(
                _, //None,
                _, //ClassTypeSignature(Some("java/lang/"), SimpleClassTypeSignature("Object", None), List()),
                // we match the (indirect) subclasses of the interface later on...
                superInterfacesSignature
                ) ← subtypeClassFile.classSignature

            componentType ← superInterfacesSignature.collectFirst {
                // 1. the hard way....
                //                case ClassTypeSignature(
                //                    Some(`containerPackageName`),
                //                    SimpleClassTypeSignature(
                //                        `containerSimpleName`,
                //                        List(
                //                            ProperTypeArgument(
                //                                None,
                //                                ClassTypeSignature(
                //                                    Some(packageName),
                //                                    SimpleClassTypeSignature(
                //                                        /*"Integer"*/ simpleName,
                //                                        _ /* None*/ ),
                //                                    Nil //List()
                //                                    )
                //                                )
                //                            )
                //                        ),
                //                    _ //List()
                //                    ) ⇒
                //                    ObjectType(packageName + simpleName)
                //
                // 2. using a custom (specialized) matcher
                case SimpleGenericType(`containerType`, componentType) ⇒ componentType
            }
        } yield {
            (subtype, componentType)
        }

        val allAffectedSubtypes =
            typeBindingSubtypes.foldLeft(Set.empty[(ObjectType, ObjectType)]) { (s, t) ⇒
                val (subtype, componentType) = t
                s ++ theProject.classHierarchy.allSubtypes(subtype, true).map((_, componentType))
            }

        val allAffectedSubtypesAsStrings =
            allAffectedSubtypes.map(p ⇒
                p._1.toJava+" inherits from "+containerType.toJava+"<"+p._2.toJava+">")

        BasicReport(
            allAffectedSubtypesAsStrings.mkString(
                "Implementations of "+containerName+":\n",
                "\n\n",
                "\nFound: "+allAffectedSubtypes.size+"("+subtypes.size+")"
            )
        )
    }
}