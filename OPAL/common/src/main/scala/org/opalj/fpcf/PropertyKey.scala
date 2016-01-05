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
package org.opalj.fpcf

import java.util.concurrent.locks.ReentrantReadWriteLock
import scala.collection.mutable.ArrayBuffer
import org.opalj.concurrent.Locking.withReadLock
import org.opalj.concurrent.Locking.withWriteLock

/**
 * A value object that identifies a specific kind of properties. Every entity in
 * the [[PropertyStore]] must be associated with at most one property per property kind/key.
 *
 * To create a property key use the companion object's [[PropertyKey$]].`create` method.
 *
 * @author Michael Eichberg
 */
class PropertyKey private[fpcf] ( final val id: Int) extends AnyVal with PropertyKind {

    override def toString: String = s"PropertyKey(${PropertyKey.name(id)},id=$id)"
}

/**
 * Factory to create [[PropertyKey]] objects.
 *
 * @author Michael Eichberg
 */
object PropertyKey {

    private[this] val lock = new ReentrantReadWriteLock

    private[this] val propertyKeyNames = ArrayBuffer.empty[String]
    private[this] val fallbackProperties = ArrayBuffer.empty[Entity ⇒ Property]
    private[this] val cycleResolutionStrategies = ArrayBuffer.empty[Iterable[EPK] ⇒ Iterable[PropertyComputationResult]]
    private[this] var lastKeyId: Int = -1

    def create(
        name:                    String,
        fallbackProperty:        Entity ⇒ Property,
        cycleResolutionStrategy: Iterable[EPK] ⇒ Iterable[PropertyComputationResult]
    ): PropertyKey = {
        withWriteLock(lock) {
            lastKeyId += 1
            propertyKeyNames += name
            fallbackProperties += fallbackProperty
            cycleResolutionStrategies += cycleResolutionStrategy
            new PropertyKey(lastKeyId)
        }
    }

    def create(
        name:                    String,
        fallback:                Property,
        cycleResolutionStrategy: Iterable[EPK] ⇒ Iterable[PropertyComputationResult]
    ): PropertyKey = {
        create(name, (e: Entity) ⇒ fallback, cycleResolutionStrategy)
    }

    def create(
        name:                    String,
        fallback:                Entity ⇒ Property,
        cycleResolutionProperty: Property
    ): PropertyKey = {
        create(
            name,
            fallback,
            (epks: Iterable[EPK]) ⇒ { Seq(Result(epks.head.e, cycleResolutionProperty)) }
        )
    }

    def create(
        name:                    String,
        fallback:                Property,
        cycleResolutionProperty: Property
    ): PropertyKey = {
        create(name, (e: Entity) ⇒ fallback, cycleResolutionProperty)
    }

    def create(name: String, fallback: Property): PropertyKey = {
        create(name, fallback, fallback)
    }

    //
    // Query the core properties of each property kind
    // ===============================================
    //

    def name(id: Int): String = withReadLock(lock) { propertyKeyNames(id) }

    def fallbackProperty(e: Entity, pk: PropertyKey): Property = {
        withReadLock(lock) {
            fallbackProperties(pk.id)(e)
        }
    }

    def resolveCycle(epks: Iterable[EPK]): Iterable[PropertyComputationResult] = {
        withReadLock(lock) {
            val epk = epks.head
            cycleResolutionStrategies(epk.pk.id)(epks)
        }
    }

    private[fpcf] def maxId = withReadLock(lock) { lastKeyId }

}
