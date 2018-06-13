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
package analyses

import org.opalj.br.DeclaredMethod
import org.opalj.br.analyses.SomeProject
import org.opalj.br.analyses.DeclaredMethodsKey
import org.opalj.fpcf.properties.VirtualMethodAllocationFreeness
import org.opalj.fpcf.properties.AllocationFreeness
import org.opalj.fpcf.properties.MethodWithAllocations
import org.opalj.fpcf.properties.AllocationFreeMethod
import org.opalj.fpcf.properties.VirtualMethodAllocationFreeness.VMethodWithAllocations
import org.opalj.fpcf.properties.VirtualMethodAllocationFreeness.VAllocationFreeMethod

/**
 * Determines the aggregated allocation freeness for virtual methods.
 *
 * @author Dominik Helm
 */
class VirtualMethodAllocationFreenessAnalysis private[analyses] ( final val project: SomeProject) extends FPCFAnalysis {
    private[this] val declaredMethods = project.get(DeclaredMethodsKey)

    def determineAllocationFreeness(dm: DeclaredMethod): PropertyComputationResult = {
        if (!dm.hasDefinition) return Result(dm, VMethodWithAllocations);

        val m = dm.methodDefinition
        var dependees: Set[EOptionP[DeclaredMethod, AllocationFreeness]] = Set.empty

        val cfo = if (dm.declaringClassType.isArrayType) project.ObjectClassFile
        else project.classFile(dm.declaringClassType.asObjectType)
        val methods =
            if (cfo.isDefined && cfo.get.isInterfaceDeclaration)
                project.interfaceCall(dm.declaringClassType.asObjectType, m.name, m.descriptor)
            else
                project.virtualCall(
                    m.classFile.thisType.packageName, dm.declaringClassType, m.name, m.descriptor
                )

        for (method ← methods) {
            propertyStore(declaredMethods(method), AllocationFreeness.key) match {
                case FinalEP(_, AllocationFreeMethod)  ⇒
                case FinalEP(_, MethodWithAllocations) ⇒ return Result(dm, VMethodWithAllocations);
                case epk                               ⇒ dependees += epk
            }
        }

        def c(eps: SomeEPS): PropertyComputationResult = {
            dependees = dependees.filter { _.e ne eps.e }

            eps match {
                case FinalEP(_, AllocationFreeMethod)  ⇒
                case FinalEP(_, MethodWithAllocations) ⇒ return Result(dm, VMethodWithAllocations);
                case epk ⇒
                    dependees += epk.asInstanceOf[EOptionP[DeclaredMethod, AllocationFreeness]]
            }

            if (dependees.isEmpty) {
                Result(dm, VAllocationFreeMethod)
            } else {
                IntermediateResult(dm, VMethodWithAllocations, VAllocationFreeMethod, dependees, c)
            }
        }

        if (dependees.isEmpty) {
            Result(dm, VAllocationFreeMethod)
        } else {
            IntermediateResult(dm, VMethodWithAllocations, VAllocationFreeMethod, dependees, c)
        }
    }

    /** Called when the analysis is scheduled lazily. */
    def doDetermineAllocationFreeness(e: Entity): PropertyComputationResult = {
        e match {
            case m: DeclaredMethod ⇒ determineAllocationFreeness(m)
            case _ ⇒ throw new UnknownError(
                "virtual method allocation freeness is only defined for declared methods"
            )
        }
    }

}

trait VirtualMethodAllocationFreenessAnalysisScheduler extends ComputationSpecification {
    override def derives: Set[PropertyKind] = Set(VirtualMethodAllocationFreeness)

    override def uses: Set[PropertyKind] = Set(AllocationFreeness)
}

object EagerVirtualMethodAllocationFreenessAnalysis
    extends VirtualMethodAllocationFreenessAnalysisScheduler with FPCFEagerAnalysisScheduler {

    def start(project: SomeProject, propertyStore: PropertyStore): FPCFAnalysis = {
        val analysis = new VirtualMethodAllocationFreenessAnalysis(project)
        val vms = project.get(DeclaredMethodsKey)
        propertyStore.scheduleEagerComputationsForEntities(vms.declaredMethods)(analysis.determineAllocationFreeness)
        analysis
    }
}

object LazyVirtualMethodAllocationFreenessAnalysis
    extends VirtualMethodAllocationFreenessAnalysisScheduler with FPCFLazyAnalysisScheduler {
    def startLazily(p: SomeProject, ps: PropertyStore): FPCFAnalysis = {
        val analysis = new VirtualMethodAllocationFreenessAnalysis(p)
        ps.registerLazyPropertyComputation(
            VirtualMethodAllocationFreeness.key,
            analysis.doDetermineAllocationFreeness
        )
        analysis
    }
}