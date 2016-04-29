/* BSD 2-Clause License:
 * Copyright (c) 2009 - 2014
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
package ai
package domain

import java.net.URL
import org.junit.runner.RunWith
import org.opalj.br.reader.{BytecodeInstructionsCache, Java8FrameworkWithCaching}
import org.scalatest.junit.JUnitRunner
import org.scalatest.Matchers
import org.opalj.br.analyses.Project
import org.opalj.br.Method
import org.scalatest.FunSpec
import scala.collection.JavaConverters._
import org.opalj.util.PerformanceEvaluation
import org.opalj.util.PerformanceEvaluation.time
import org.opalj.br.analyses.MethodInfo

/**
 * Tests if we are able to computed the CFG as well as the dominator/post-dominator tree for
 * a larger number of classes.
 *
 * @author Michael Eichberg
 */
@RunWith(classOf[JUnitRunner])
class RecordCFGTest extends FunSpec with Matchers {

    object DominatorsPerformanceEvaluation extends PerformanceEvaluation

    object PostDominatorsPerformanceEvaluation extends PerformanceEvaluation

    class RecordCFGDomain[I](val method: Method, val project: Project[URL])
        extends CorrelationalDomain
        with TheProject
        with TheMethod
        with ProjectBasedClassHierarchy
        with ThrowAllPotentialExceptionsConfiguration
        with DefaultHandlingOfMethodResults
        with IgnoreSynchronization
        with l1.DefaultReferenceValuesBinding
        with l1.NullPropertyRefinement
        with l0.DefaultTypeLevelIntegerValues
        with l0.DefaultTypeLevelLongValues
        with l0.DefaultTypeLevelFloatValues
        with l0.DefaultTypeLevelDoubleValues
        with l0.TypeLevelPrimitiveValuesConversions
        with l0.TypeLevelInvokeInstructions
        with l0.TypeLevelFieldAccessInstructions
        with l0.TypeLevelLongValuesShiftOperators
        with RecordCFG // <=== the domain we are going to test!

    def analyzeProject(name: String, project: Project[java.net.URL]): Unit = {
        info(s"the loaded project ($name) contains ${project.methodsCount} methods")

        val failures = new java.util.concurrent.ConcurrentLinkedQueue[(String, Throwable)]

        project.parForeachMethodWithBody() { methodInfo ⇒
            val MethodInfo(_, classFile, method) = methodInfo

            try {
                val domain = new RecordCFGDomain(method, project)
                val evaluatedInstructions = DominatorsPerformanceEvaluation.time('AI) {
                    val r = BaseAI(classFile, method, domain)
                    r.evaluatedInstructions
                }
                evaluatedInstructions.foreach { pc ⇒

                    domain.foreachSuccessorOf(pc) { succPC ⇒
                        domain.predecessorsOf(succPC).contains(pc) should be(true)
                    }

                    domain.foreachPredecessorOf(pc) { predPC ⇒
                        domain.allSuccessorsOf(predPC).contains(pc) should be(true)
                    }
                }
                val dt = DominatorsPerformanceEvaluation.time('Dominators) {
                    domain.dominatorTree
                }
                val postDT = PostDominatorsPerformanceEvaluation.time('PostDominators) {
                    domain.postDominatorTree
                }
                evaluatedInstructions foreach { pc ⇒
                    if (pc != dt.startNode && !evaluatedInstructions.contains(dt.dom(pc))) {
                        fail(s"the dominator ${dt.dom(pc)} of $pc was not evaluated")
                    }
                    if (pc != postDT.startNode &&
                        postDT.dom(pc) != postDT.startNode &&
                        !evaluatedInstructions.contains(postDT.dom(pc))) {
                        fail(s"the post-dominator ${postDT.dom(pc)} of $pc was not evaluated")
                    }
                }
            } catch {
                case t: Throwable ⇒
                    val methodName = method.toJava(classFile)
                    failures.add((methodName, t))
            }
        }

        if (failures.size > 0) {
            val failureMessages = for { (failure, exception) ← failures.asScala } yield {
                var root: Throwable = exception
                while (root.getCause != null) root = root.getCause
                val location =
                    if (root.getStackTrace() != null && root.getStackTrace().length > 0) {
                        root.getStackTrace().take(5).map { stackTraceElement ⇒
                            stackTraceElement.getClassName+
                                " { "+
                                stackTraceElement.getMethodName+":"+stackTraceElement.getLineNumber+
                                " }"
                        }.mkString("; ")
                    } else {
                        "<location unavailable>"
                    }
                failure+" ["+root.getClass.getSimpleName+": "+root.getMessage+"; location: "+location+"] "
            }

            val failuresHeader = s"${failures.size} exceptions occured in: \n"
            val failuresInfo = failureMessages.mkString(failuresHeader, "\n", "\n")
            fail(failuresInfo)
        }
    }

    describe("calculating the (post)dominator trees") {
        def printPerformanceData(): Unit = {
            {
                import DominatorsPerformanceEvaluation.getTime
                info("performing AI took (CPU time) "+getTime('AI).toSeconds)
                info("computing dominator information took (CPU time)"+getTime('Dominators).toSeconds)
            }

            {
                import PostDominatorsPerformanceEvaluation.getTime
                val duration = getTime('PostDominators).toSeconds
                info("computing post-dominator information took (CPU time) "+duration)
            }
        }

        val reader = new Java8FrameworkWithCaching(new BytecodeInstructionsCache)
        import reader.AllClassFiles

        it("should be possible to calculate the (post)dominator trees for all methods of the JDK") {
            DominatorsPerformanceEvaluation.resetAll()
            PostDominatorsPerformanceEvaluation.resetAll()

            val project = org.opalj.br.TestSupport.createJREProject

            time { analyzeProject("JDK", project) } { t ⇒ info("the analysis took (real time)"+t.toSeconds) }

            printPerformanceData()
        }

        it("should be possible to calculate the (post)dominator trees for all methods of the OPAL 0.3 snapshot") {
            DominatorsPerformanceEvaluation.resetAll()
            PostDominatorsPerformanceEvaluation.resetAll()

            val classFiles = org.opalj.bi.TestSupport.locateTestResources("classfiles/OPAL-SNAPSHOT-0.3.jar", "bi")
            val project = Project(reader.ClassFiles(classFiles), Traversable.empty, true)

            time { analyzeProject("OPAL-0.3", project) } { t ⇒ info("the analysis took (real time)"+t.toSeconds) }

            printPerformanceData()
        }

        it("should be possible to calculate the (post)dominator trees for all methods of the OPAL-08-14-2014 snapshot") {
            DominatorsPerformanceEvaluation.resetAll()
            PostDominatorsPerformanceEvaluation.resetAll()

            val classFilesFolder = org.opalj.bi.TestSupport.locateTestResources("classfiles", "bi")
            val opalJARs = classFilesFolder.listFiles(new java.io.FilenameFilter() {
                def accept(dir: java.io.File, name: String) =
                    name.startsWith("OPAL-") && name.contains("SNAPSHOT-08-14-2014")
            })
            info(opalJARs.mkString("analyzing the following jars: ", ", ", ""))
            opalJARs.size should not be (0)
            val project = Project(AllClassFiles(opalJARs), Traversable.empty, true)

            time {
                analyzeProject("OPAL-08-14-2014 snapshot", project)
            } { t ⇒ info("the analysis took (real time) "+t.toSeconds) }

            printPerformanceData()
        }

    }
}