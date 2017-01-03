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
package org.opalj
package ba

import org.opalj.collection.immutable.UShortPair

/**
 * ClassFileBuilder for the parameters specified after Fields or Methods have been added.
 *
 * @author Malte Limmeroth
 * @author Michael Eichberg
 */
class ClassFileBuilder(
        private var version:        UShortPair                      = ClassFileBuilder.DefaultVersion,
        private var accessFlags:    Int                             = 0,
        private var thisType:       br.ObjectType                   = null, // REQUIRED
        private var superclassType: Option[br.ObjectType]           = None,
        private var interfaceTypes: Seq[br.ObjectType]              = Seq.empty,
        private var fields:         br.Fields                       = IndexedSeq.empty,
        private var methods:        br.Methods                      = IndexedSeq.empty,
        private var attributes:     br.Attributes                   = IndexedSeq.empty,
        private var annotations:    Map[br.Method, Map[br.PC, Any]] = Map.empty
) {

    /**
     * Defines the extending class.
     *
     * @param fqn The extending class name in JVM notation as a fully qualified name, e.g.
     *            "java/lang/Object".
     */
    def EXTENDS(fqn: String): this.type = {
        superclassType = Some(br.ObjectType(fqn))

        this
    }

    /**
     * Defines the implemented interfaces.
     *
     * @param fqn The interfaces class names in JVM notation as a fully qualified name, e.g.
     *            "java/lang/Object".
     */
    def IMPLEMENTS(fqns: String*): this.type = {
        interfaceTypes = fqns.map(br.ObjectType.apply)

        this
    }

    /**
     * Defines the minorVersion and majorVersion. The default values are the current values.
     */
    def Version(minorVersion: Int = version.minor, majorVersion: Int = version.major): this.type = {
        version = UShortPair(minorVersion, majorVersion)

        this
    }

    /**
     * Defines the SourceFile attribute.
     */
    def SourceFile(sourceFile: String): this.type = {
        attributes :+= br.SourceFile(sourceFile)

        this
    }

    /**
     * Builds the [[org.opalj.br.ClassFile]] given the current information.
     *
     * The following conditional changes are done to ensure a correct class file is created:
     *  - For regular classes (not interface types) a default constructor will be generated
     *    if no constructor was defined and the superclass type information is available.
     */
    def buildBRClassFile(): (br.ClassFile, Map[br.Method, Map[br.PC, Any]]) = {
        if (!(
            bi.ACC_INTERFACE.isSet(accessFlags) ||
            methods.exists(_.isConstructor) ||
            // If "only" the following partical condition holds,
            // the the class file will be invalid, but we can't 
            // generate a default constructor, because we don't 
            // know the target!
            superclassType.isEmpty
        )) {
            this.methods :+= br.Method.defaultConstructor(this.superclassType.get)
        }
        val classFile = br.ClassFile( // <= THE FACTORY METHOD ENSURES THAT THE MEMBERS ARE SORTED
            version.minor,
            version.major,
            accessFlags,
            thisType,
            superclassType,
            interfaceTypes,
            fields,
            methods,
            attributes
        )
        (classFile, annotations)
    }

    /**
     * Returns the build [[org.opalj.da.ClassFile]].
     *
     * @see [[buildBRClassFile]]
     */
    def buildDAClassFile(): (da.ClassFile, Map[br.Method, Map[br.PC, Any]]) = {
        val (brClassFile, annotations) = buildBRClassFile()
        (toDA(brClassFile), annotations)
    }

}

object ClassFileBuilder {

    final val DefaultMajorVersion = 50

    final val DefaultMinorVersion = 0

    final val DefaultVersion = UShortPair(DefaultMinorVersion, DefaultMajorVersion)

}