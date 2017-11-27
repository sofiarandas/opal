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
package fpcf

import java.net.URL

import org.opalj.br.Method
import org.opalj.br.analyses.PropertyStoreKey
import org.opalj.br.analyses.DefaultOneStepAnalysis
import org.opalj.br.analyses.BasicReport
import org.opalj.br.analyses.Project
import org.opalj.fpcf.properties.AllThrownExceptions
import org.opalj.fpcf.properties.ThrownExceptions
import org.opalj.fpcf.properties.ThrownExceptionsFallbackAnalysis
import org.opalj.fpcf.properties.NoExceptionsAreThrown.MethodIsAbstract

/**
 * Prints out the information about the exceptions thrown by methods.
 *
 * @author Michael Eichberg
 */
object ThrownExceptionsRunner extends DefaultOneStepAnalysis {

    override def description: String = "approximates the set of exceptions thrown by methods"

    def doAnalyze(
        project:       Project[URL],
        parameters:    Seq[String],
        isInterrupted: () ⇒ Boolean
    ): BasicReport = {
        val ps = project.get(PropertyStoreKey)
        ps.scheduleForEntities(project.allMethods)(new ThrownExceptionsFallbackAnalysis(ps))
        ps.waitOnPropertyComputationCompletion(true)

        val methodsWithCompleteThrownExceptionsInfo = ps.entities(ThrownExceptions.Key).collect {
            case EP(m: Method, ts: AllThrownExceptions) if ts != MethodIsAbstract ⇒ (m, ts)
        }
        val methodsWhichDoNotThrowExceptions = methodsWithCompleteThrownExceptionsInfo.collect {
            case e @ (m: Method, ts: AllThrownExceptions) if ts.types.isEmpty ⇒ e
        }

        val methodsWithCompleteThrownExceptionsInfoCount =
            methodsWithCompleteThrownExceptionsInfo.size
        val privateMethodsWithCompleteThrownExceptionsInfoCount =
            methodsWithCompleteThrownExceptionsInfo.view.filter(_._1.isPrivate).size
        val methodsWhichDoNotThrowExceptionsCount =
            methodsWhichDoNotThrowExceptions.view.filter(_._1.isPrivate).size

        val report = methodsWithCompleteThrownExceptionsInfo.map {
            case (m: Method, ts: AllThrownExceptions) ⇒ { s"${m.toJava} ⇒ $ts" }
        }.toList.sorted.mkString("\n")

        BasicReport(
            report+
                "\n\nNumber of methods for which the set of thrown exceptions is known: "+
                methodsWithCompleteThrownExceptionsInfoCount+"\n"+
                s" ... private methods: ${privateMethodsWithCompleteThrownExceptionsInfoCount}\n"+
                s" ... which throw no exceptions: ${methodsWhichDoNotThrowExceptions.size}\n"+
                s" ... ... private methods: $methodsWhichDoNotThrowExceptionsCount"
        )
    }
}