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
package ai

import org.opalj.br.ReferenceType
import org.opalj.br.ClassHierarchy

/**
 * Makes a project's class hierarchy available to a `Domain`.
 *
 * Implements a Domain's `isSubtypeOf(...)` by delegating to
 * the corresponding method defined in [[org.opalj.br.ClassHierarchy]].
 *
 * @author Michael Eichberg
 */
trait TheClassHierarchy {

    /**
     * This project's class hierarchy.
     *
     * Usually, just a redirect to the `Project`'s class hierarchy or the
     * default class hierarchy.
     */
    implicit def classHierarchy: ClassHierarchy

    /**
     * @see [[Domain.isSubtypeOf]]
     *
     * @see Delegates to [[org.opalj.br.ClassHierarchy]]'s `isSubtypeOf` method.
     */
    /*override*/ def isSubtypeOf(subtype: ReferenceType, supertype: ReferenceType): Answer = {
        classHierarchy.isSubtypeOf(subtype, supertype)
    }

}