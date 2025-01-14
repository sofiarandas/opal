/* BSD 2-Clause License - see OPAL/LICENSE for details. */
package org.opalj
package br
package fpcf
package properties

import org.opalj.fpcf.Property
import org.opalj.fpcf.PropertyKey
import org.opalj.fpcf.PropertyMetaInformation

sealed trait AliasPropertyMetaInformation extends PropertyMetaInformation {
    type Self = Alias
}

/**
 * Describes the alias properties of the associated entities.
 *
 * Alias properties can establish a binding between a pair of entities, including definition sites, formal parameters, method return values, fields, etc.
 * For more information, see TODO.
 * The pair may consist of different types of entities, such as a field and a formal parameter.
 *
 * An alias property provides information about the relationship of the memory locations of the associated entities.
 *
 *  - [[NoAlias]] can be assigned to a pair of entities, iff no execution path exists where both entities refer to the same memory location.
 *   This implies that there is a definite separation of the memory locations of the two entities, and no interference can occur when one of the entities is referenced.
 *   Note that this does not imply that, for example, the fields of the entities cannot refer to the same memory location.
 *   Example:
 *   {{{
 *     Object m(Object a) {
 *       Object b = new Object();
 *       return b;
 *     }
 *   }}}
 *   In this example, the formal parameter `a` and the local variable `b` are guaranteed to never refer to the same memory location,
 *   as `b` is always assigned to a new object that the formal parameter can't possibly be pointing to since the object has not been created before the method call.
 *   This ensures that the method's return value and the formal parameter never point to the same memory location.
 *
 *  - [[MustAlias]] can be assigned to a pair of entities, iff both entities refer to the same memory location in every execution time at all times.
 *   This implies that at every use site of one entity, it can be replaced with the other entity (if it is defined at the current location) without changing the semantics of the program.
 *   Example:
 *   {{{
 *    Object m() {
 *      Object a = new Object();
 *      Object b = a;
 *      return b;
 *    }
 *   }}}
 *   In this example, the local variable `a` and `b` are guaranteed to refer to the same memory location at all times,
 *   because a is only assigned once and b is only defined once with the value of a.
 *   This means that the return value of the method can be replaced with `a`.
 *
 *  - [[MayAlias]] can always be assigned to any given pair of entities without invalidating the results.
 *   It indicates that the two entities might refer to the same memory location but are not obligated to do so.
 *   This serves as a default/fallback value when no other property can be guaranteed.
 *   Example:
 *   {{{
 *     Object m(Object a) {
 *       Object b = a;
 *       System.out.println(b);
 *       b = new Object();
 *       return b;
 *     }
 *   }}}
 *   In this example neither [[MustAlias]] nor [[NoAlias]] can be assigned to the pair of formal parameter `a` and local variable `b`,
 *   because b initially refers to the same memory location as a but is later assigned to a new object.
 *
 * Note that the examples above are usually already optimized by the compiler and are used for illustrative purposes.
 *
 * An analysis should attempt to be as sound as possible when assigning [[MustAlias]] or [[NoAlias]] properties,
 * but might not always be able to do so, e.g. when a field is changed via reflection or native methods.
 * In such cases, the analysis should document the possible unsoundness.
 *
 * Alias information is only defined at a location where both entities of the associated pair are defined.
 * If one of the entities is not defined at the current location, the given alias property holds no information.
 */
sealed trait Alias extends AliasPropertyMetaInformation with Property {

    /**
     * A globally unique key used to access alias properties
     */
    final def key: PropertyKey[Alias] = Alias.key
}

object Alias extends AliasPropertyMetaInformation {

    /**
     * The name of the alias [[key]].
     */
    final val PropertyKeyName = "opalj.Alias"

    /**
     * The key used to access alias properties. It's name is "opalj.Alias" and the fallback value is [[MayAlias]].
     */
    final val key: PropertyKey[Alias] = {
        PropertyKey.create(
            PropertyKeyName,
            MayAlias
        )
    }
}

/**
 * Indicates that the two associated entities are guaranteed to '''always''' refer to the same memory location.
 *
 * @see [[Alias]] for more information about alias properties.
 */
case object MustAlias extends Alias

/**
 * Indicates that the two associated entities are guaranteed to '''never''' refer to the same memory location.
 *
 * @see [[Alias]] for more information about alias properties.
 */
case object NoAlias extends Alias

/**
 * Indicates that the two associated entities might refer to the same memory location but are not obligated to do so.
 *
 * This property does not guarantee that the actually relation of the associated entities can't be described using [[MustAlias]] or [[MayAlias]],
 * as there are scenarios where the analysis may not have sufficient information to prove a more specific relationship
 *
 * @see [[Alias]] for more information about alias properties.
 */
case object MayAlias extends Alias
