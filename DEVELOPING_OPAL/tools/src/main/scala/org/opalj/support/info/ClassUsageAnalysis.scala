/* BSD 2-Clause License - see OPAL/LICENSE for details. */
package org.opalj.support.info

import scala.annotation.switch

import java.net.URL
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicInteger

import scala.collection.mutable.ListBuffer

import org.opalj.log.GlobalLogContext
import org.opalj.log.OPALLogger
import org.opalj.value.ValueInformation
import org.opalj.br.analyses.BasicReport
import org.opalj.br.analyses.DefaultOneStepAnalysis
import org.opalj.br.analyses.Project
import org.opalj.br.analyses.ReportableAnalysisResult
import org.opalj.tac.Assignment
import org.opalj.tac.Call
import org.opalj.tac.DUVar
import org.opalj.tac.ExprStmt
import org.opalj.tac.SimpleTACAIKey
import org.opalj.tac.VirtualFunctionCall

/**
 * Analyzes a project for how a particular class is used within that project. This means that this
 * analysis collect information on which methods are called on objects of that class as well as how
 * often.
 *
 * The analysis can be configured by passing the following optional parameters: `class` (the class
 * to analyze), `granularity` (fine- or coarse-grained; defines which information will be gathered
 * by an analysis run). For further information see
 * [[ClassUsageAnalysis.analysisSpecificParametersDescription]].
 *
 * @author Patrick Mell
 */
object ClassUsageAnalysis extends DefaultOneStepAnalysis {

    private type V = DUVar[ValueInformation]

    implicit val logContext: GlobalLogContext.type = GlobalLogContext

    override def title: String = "Class Usage Analysis"

    override def description: String = {
        "Analyzes a project for how a particular class is used within it, i.e., which methods "+
            "of instances of that class are called"
    }

    /**
     * The fully-qualified name of the class that is to be analyzed in a Java format, i.e., dots as
     * package / class separators.
     */
    private var className = "java.lang.StringBuilder"

    /**
     * The analysis can run in two modes: Fine-grained or coarse-grained. Fine-grained means that
     * two methods are considered equal iff their method descriptor is the same, i.e., this mode
     * enables a differentiation between overloaded methods.
     * The coarse-grained method, however, regards two method calls as the same if the class of the
     * base object as well as the method name are equal, i.e., overloaded methods are not
     * distinguished.
     */
    private var isFineGrainedAnalysis = false

    /**
     * Takes a [[Call]] and assembles the method descriptor for this call. The granularity is
     * determined by [[isFineGrainedAnalysis]]: For a fine-grained analysis, the returned string has
     * the format "[fully-qualified classname]#[method name]: [stringified method descriptor]" and
     * for a coarse-grained analysis: [fully-qualified classname]#[method name].
     */
    private def assembleMethodDescriptor(call: Call[V]): String = {
        val fqMethodName = s"${call.declaringClass.toJava}#${call.name}"
        if (isFineGrainedAnalysis) {
            val methodDescriptor = call.descriptor.toString
            s"$fqMethodName: $methodDescriptor"
        } else {
            fqMethodName
        }
    }

    /**
     * Takes any function [[Call]], checks whether the base object is of type [[className]] and if
     * so, updates the passed map by adding the count of the corresponding method. The granularity
     * for counting is determined by [[isFineGrainedAnalysis]].
     */
    private def processFunctionCall(
        call: Call[V], map: ConcurrentHashMap[String, AtomicInteger]
    ): Unit = {
        val declaringClassName = call.declaringClass.toJava
        if (declaringClassName == className) {
            val methodDescriptor = assembleMethodDescriptor(call)
            if (map.putIfAbsent(methodDescriptor, new AtomicInteger(1)) != null) {
                map.get(methodDescriptor).addAndGet(1)
            }
        }
    }

    override def analysisSpecificParametersDescription: String = {
        "[-class=<fully-qualified class name>  (Default: java.lang.StringBuilder)]\n"+
            "[-granularity=<fine|coarse> (Default: coarse)]"
    }

    private final val parameterNameForClass = "-class="
    private final val parameterNameForGranularity = "-granularity="

    override def checkAnalysisSpecificParameters(parameters: Seq[String]): Traversable[String] = {
        val remainingParameters =
            parameters.filter { p ⇒
                !p.contains(parameterNameForClass) && !p.contains(parameterNameForGranularity)
            }
        super.checkAnalysisSpecificParameters(remainingParameters)
    }

    /**
     * Takes the parameters passed as program arguments, i.e., in the format
     * "-[param name]=[value]", extracts the values and sets the corresponding object variables.
     */
    private def setAnalysisParameters(parameters: Seq[String]): Unit = {
        val classParam = parameters.find(_.startsWith(parameterNameForClass))
        if (classParam.isDefined) {
            className = classParam.get.substring(classParam.get.indexOf("=") + 1)
        }

        val granularityParam = parameters.find(_.startsWith(parameterNameForGranularity))
        if (granularityParam.isDefined) {
            val granularity = granularityParam.get.substring(granularityParam.get.indexOf("=") + 1)
            if (granularity == "fine") {
                isFineGrainedAnalysis = true
            } else if (granularity == "coarse") {
                isFineGrainedAnalysis = false
            } else {
                val errMsg = s"failed parsing the granularity; it must be either 'fine' or "+
                    s"'coarse' but got '$granularity'"
                OPALLogger.error("fatal", errMsg)
                sys.exit(2)
            }
        }
    }

    override def doAnalyze(
        project: Project[URL], parameters: Seq[String], isInterrupted: () ⇒ Boolean
    ): ReportableAnalysisResult = {
        setAnalysisParameters(parameters)
        val resultMap: ConcurrentHashMap[String, AtomicInteger] = new ConcurrentHashMap
        val tacProvider = project.get(SimpleTACAIKey)

        project.parForeachMethodWithBody() { methodInfo ⇒
            tacProvider(methodInfo.method).stmts.foreach { stmt ⇒
                (stmt.astID: @switch) match {
                    case Assignment.ASTID ⇒ stmt match {
                        case Assignment(_, _, c: VirtualFunctionCall[V]) ⇒
                            processFunctionCall(c, resultMap)
                        case _ ⇒
                    }
                    case ExprStmt.ASTID ⇒ stmt match {
                        case ExprStmt(_, c: VirtualFunctionCall[V]) ⇒
                            processFunctionCall(c, resultMap)
                        case _ ⇒
                    }
                    case _ ⇒
                }
            }
        }

        val report = ListBuffer[String]("Result:")
        // Transform to a list, sort in ascending order of occurrences, and format the information
        resultMap.entrySet().stream().sorted { (value1, value2) ⇒
            value1.getValue.get().compareTo(value2.getValue.get())
        }.forEach(next ⇒ report.append(s"${next.getKey}: ${next.getValue}"))
        BasicReport(report)
    }

}
