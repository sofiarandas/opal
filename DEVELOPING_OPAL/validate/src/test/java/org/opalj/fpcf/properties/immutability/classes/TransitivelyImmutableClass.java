/* BSD 2-Clause License - see OPAL/LICENSE for details. */
package org.opalj.fpcf.properties.immutability.classes;

import java.lang.annotation.Documented;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;

import org.opalj.br.fpcf.FPCFAnalysis;
import org.opalj.fpcf.properties.PropertyValidator;
import org.opalj.br.fpcf.analyses.immutability.ClassImmutabilityAnalysis;

/**
 * Annotation to state that the annotated class is transitively immutable.
 *
 * @author Tobias Roth
 */
@PropertyValidator(key = "ClassImmutability",validator = TransitivelyImmutableClassMatcher.class)
@Documented
@Retention(RetentionPolicy.CLASS)
public @interface TransitivelyImmutableClass {

    /**
     * A short reasoning of this property.
     */
    String value();

    Class<? extends FPCFAnalysis>[] analyses() default {ClassImmutabilityAnalysis.class};

}