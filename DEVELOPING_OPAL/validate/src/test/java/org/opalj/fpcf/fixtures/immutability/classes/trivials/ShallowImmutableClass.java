package org.opalj.fpcf.fixtures.immutability.classes.trivials;

import org.opalj.fpcf.properties.class_immutability.ShallowImmutableClassAnnotation;
import org.opalj.fpcf.properties.field_immutability.ShallowImmutableFieldAnnotation;
import org.opalj.fpcf.properties.reference_immutability.ImmutableReferenceAnnotation;
import org.opalj.fpcf.properties.type_immutability.MutableTypeAnnotation;

@MutableTypeAnnotation("Because of not final class")
@ShallowImmutableClassAnnotation("has shallow immutable field")
public class ShallowImmutableClass {
    @ShallowImmutableFieldAnnotation("Because of mutable type")
    @ImmutableReferenceAnnotation("Because it is private")
    private MutableClass mutableClass = new MutableClass();
}
