/* BSD 2-Clause License - see OPAL/LICENSE for details. */
package org.opalj
package fpcf
package properties
package static_data_usage

import org.opalj.br.AnnotationLike
import org.opalj.br.ObjectType
import org.opalj.br.analyses.SomeProject

/**
 * Base trait for matchers that match a method's `StaticDataUsage` property.
 *
 * @author Dominik Helm
 */
sealed abstract class StaticDataUsageMatcher(val property: StaticDataUsage)
    extends AbstractPropertyMatcher {

    def validateProperty(
        p:          SomeProject,
        as:         Set[ObjectType],
        entity:     Entity,
        a:          AnnotationLike,
        properties: Traversable[Property]
    ): Option[String] = {
        if (!properties.exists(_ match {
            case `property` ⇒ true
            case _          ⇒ false
        })) {
            // ... when we reach this point the expected property was not found.
            Some(a.elementValuePairs.head.value.asStringValue.value)
        } else {
            None
        }
    }
}

/**
 * Matches a method's `StaticDataUsage` property. The match is successful if the method has the
 * property [[org.opalj.fpcf.properties.UsesNoStaticData]].
 */
class UsesNoStaticDataMatcher extends StaticDataUsageMatcher(properties.UsesNoStaticData)

/**
 * Matches a method's `StaticDataUsage` property. The match is successful if the method has the
 * property [[org.opalj.fpcf.properties.UsesConstantDataOnly]].
 */
class UsesConstantDataOnlyMatcher extends StaticDataUsageMatcher(properties.UsesConstantDataOnly)

/**
 * Matches a method's `StaticDataUsage` property. The match is successful if the method has the
 * property [[org.opalj.fpcf.properties.UsesVaryingData]].
 */
class UsesVaryingDataMatcher extends StaticDataUsageMatcher(properties.UsesVaryingData)