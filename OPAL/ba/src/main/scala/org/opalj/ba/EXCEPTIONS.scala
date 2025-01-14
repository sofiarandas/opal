/* BSD 2-Clause License - see OPAL/LICENSE for details. */
package org.opalj
package ba

import scala.collection.immutable.ArraySeq

import org.opalj.br.ObjectType

/**
 * Builder for the [[org.opalj.br.ExceptionTable]] attribute.
 *
 * @author Michael Eichberg
 */
case class EXCEPTIONS(exceptionTypes: String*) extends br.MethodAttributeBuilder {

    def apply(
        accessFlags: Int,
        name:        String,
        descriptor:  br.MethodDescriptor
    ): br.ExceptionTable = {
        br.ExceptionTable(
            ArraySeq.from(exceptionTypes.toArray[String]).map(ObjectType.apply)
        )
    }

}
