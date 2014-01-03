/* License (BSD Style License):
 * Copyright (c) 2009 - 2013
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
 *  - Neither the name of the Software Technology Group or Technische
 *    Universität Darmstadt nor the names of its contributors may be used to
 *    endorse or promote products derived from this software without specific
 *    prior written permission.
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
package de.tud.cs.st
package bat
package resolved
package ai
package domain
package l0

import de.tud.cs.st.util.{ Answer, Yes, No, Unknown }

/**
 * This (partial-)domain implements the foundations for performing
 * computations related to reference values.
 *
 * ==Extending/Implementing This Domain==
 * The following implementation decisions need to be taken into account when
 * inheriting from this trait:
 *  - By default equality of `DomainValue`s that represent reference values is
 *    reference based. I.e., two instances of `DomainValue`s that represent
 *    reference values are never equal. However, subclasses may implement their
 *    own strategy.
 *  - Instances of `DomainValue`s are always treated as immutable. Every
 *    update of a value's properties creates a new value. This is a general design
 *    decision underlying BATAI and should not be changed.
 *  - A new instance of a `DomainValue` is always exclusively created by one of the
 *    factory methods. (The factory methods generally start with a capital letter
 *    and are correspondingly documented.) This greatly facilitates domain adaptability
 *    and selective customizations.
 *
 * @author Michael Eichberg
 */
trait TypeLevelReferenceValues[+I]
        extends Domain[I]
        with GeneralizedArrayHandling {
    domain: Configuration with IntegerValuesComparison ⇒

    /**
     * Abstracts over all values with computational type `reference`. I.e.,
     * abstracts over class and array values and also the `null` value.
     */
    protected trait ReferenceValue extends Value with IsReferenceValue {
        this: DomainValue ⇒

        /**
         * Returns `ComputationalTypeReference`.
         */
        final override def computationalType: ComputationalType =
            ComputationalTypeReference

        /**
         * Summarizes this value and the given value by `join`ing both values and
         * returning the joined value.
         *
         * @param pc The PC that will be used by the new summarized value unless
         *      the summary returns this value.
         * @param value A value with computational type reference value.
         * @note Though the default implementation will always work, it may not provide
         *      the desired/necessary level of abstraction. In such cases, this method
         *      needs to be overridden.
         */
        override def summarize(pc: PC, value: DomainValue): DomainValue = {
            this.join(pc, value) match {
                case SomeUpdate(value) ⇒ value
                case _                 ⇒ this
            }
        }

        /**
         * Returns `Yes` iff this value is guaranteed to be `null` at runtime and
         * returns `No` iff the value is not `null` at runtime, in all other cases
         * `Unknown` is returned.
         *
         * This default implementation always returns `Unknown`.
         */
        override def isNull: Answer = Unknown

        /**
         * Returns `true` if the type information associated with this value is precise.
         * I.e., the type information associated with this value precisely models the
         * runtime type. If, `isPrecise` returns true, the type of this value can
         * generally be assumed to represent a class type (not an interface type) or
         * an array type. However, this domain also supports the case that `isPrecise`
         * returns `true` even though the associated type identifies an interface type
         * or an abstract class type. The later case may be interesting
         *
         * This default implementation always returns `false`.
         */
        override def isPrecise: Boolean = false

        /**
         * Tests if this value's type is potentially a subtype of the given type.
         * This test should take the precision of the type information into account.
         * That is, if the currently available type information is not precise and
         * the given type has a subtype that is always a subtype of the current
         * upper type bound, then `Unknown` should to be returned. Given that it may be
         * computationally intensive to determine whether two types have a common subtype
         * it may be better to just return `Unknown` in case that this type and the
         * given type are not in a direct inheritance relationship.
         *
         * This default implementation always returns `Unknown`.
         *
         * @note If this value represents the `null` value this method is not supported.
         */
        @throws[DomainException]("if this value is null")
        override def isValueSubtypeOf(referenceType: ReferenceType): Answer = Unknown

        /**
         * Adds a new, additional upper bound to this value's type.
         */
        def refineUpperTypeBound(pc: PC, supertype: ReferenceType): DomainValue

        /**
         * Updates the "null"ness property of this value.
         *
         * @note If this value represents the `null` value this method is not supported.
         */
        @throws[DomainException]("if this value is null")
        def updateIsNull(pc: PC, isNull: Answer): DomainValue

    }

    /**
     * Represents the value `null`. Null values are basically found in the following two
     * cases:
     *  1. A null value was pushed onto the stack using `aconst_null`.
     *  2. A reference value that is not guaranteed to be non-null is tested against
     *    `null` using `ifnull` or `ifnonnull` and we are now on the branch where
     *    the value has to be `null`.
     */
    protected trait NullValue extends ReferenceValue {
        this: DomainValue ⇒

        final override def referenceValues: Iterable[IsAReferenceValue] = Iterable(this)

        /**
         * Returns `Yes`.
         */
        final override def isNull = Yes

        /**
         * Returns `true`.
         */
        final override def isPrecise = true

        /**
         * Returns an empty upper type bound.
         */
        final override def upperTypeBound: UpperTypeBound = UIDList.empty

        /**
         * Throws a new `DomainException` that states that this method is not supported.
         */
        final override def updateIsNull(pc: PC, isNull: Answer): Nothing =
            domainException(domain, "this value is null; changing that doesn't make sense")

        /**
         * Throws a new `DomainException` that states that this method is not supported.
         */
        final override def isValueSubtypeOf(referenceType: ReferenceType): Nothing =
            domainException(domain, "isSubtypeOf is not defined for \"null\" values")

        override def refineUpperTypeBound(
            pc: PC,
            supertype: ReferenceType): DomainValue = this

        override def summarize(pc: PC): DomainValue = this

        override def adapt[ThatI >: I](
            target: Domain[ThatI],
            pc: PC): target.DomainValue =
            target.NullValue(pc)

        override def toString: String = "ReferenceValue(null)"
    }

    /**
     * A reference value that is associated with a single (upper) type (bound).
     *
     * @note This class was introduced for performance reasons.
     */
    protected trait SReferenceValue[T <: ReferenceType] extends ReferenceValue {
        this: DomainValue ⇒

        val theUpperTypeBound: T

        override def referenceValues: Iterable[IsAReferenceValue] = Iterable(this)

        override def upperTypeBound: UpperTypeBound = UIDList(theUpperTypeBound)

        override def summarize(pc: PC): DomainValue = this

        override def toString: String = "ReferenceValue("+theUpperTypeBound.toJava+")"

    }

    protected trait ObjectValue extends ReferenceValue {
        this: DomainValue ⇒
    }

    protected trait ArrayValue extends ReferenceValue {
        this: DomainValue ⇒

        /**
         * Returns `Yes` if we can statically determine that the given value can
         * be stored in the array represented by this `ArrayValue`.
         */
        /*ABSTRACT*/ def isAssignable(value: DomainValue): Answer

        /*ABSTRACT*/ def doLoad(
            pc: PC,
            index: DomainValue,
            potentialExceptions: ExceptionValues): ArrayLoadResult

        def load(pc: PC, index: DomainValue): ArrayLoadResult = {
            // @note
            // The case "this.isNull == Yes" will not occur as the value "null" is always
            // represented by an instance of the respective class and this situation
            // is checked for by the domain-level method.

            val validIndex =
                isSomeValueInRange(index, IntegerConstant0, getLength(pc))
            if (validIndex.no)
                return justThrows(ArrayIndexOutOfBoundsException(pc))

            var thrownExceptions = List.empty[ExceptionValue]
            if (validIndex.maybeNo && throwArrayIndexOutOfBoundsException)
                thrownExceptions = ArrayIndexOutOfBoundsException(pc) :: thrownExceptions
            if (isNull.maybeYes && throwNullPointerException)
                thrownExceptions = NullPointerException(pc) :: thrownExceptions

            doLoad(pc, index, thrownExceptions)
        }

        def doArraystore(
            pc: PC,
            value: DomainValue,
            index: DomainValue): Unit = { /* Empty by default. */ }

        /**
         * @note It is in general not necessary to override this method. If you need some
         *      special handling if a value is stored in an array, override the method
         *      `doArraystore`.
         * @see `doArraystore` for further information.
         */
        def store(
            pc: PC,
            value: DomainValue,
            index: DomainValue): ArrayStoreResult = {
            // @note
            // The case "this.isNull == Yes" will not occur as the value "null" is always
            // represented by an instance of the respective class

            val validIndex =
                isSomeValueInRange(index, IntegerConstant0, getLength(pc))
            if (validIndex.no)
                return justThrows(ArrayIndexOutOfBoundsException(pc))

            if (isAssignable(value).no)
                return justThrows(ArrayStoreException(pc))

            var thrownExceptions = List.empty[ExceptionValue]
            if (validIndex.maybeNo && throwArrayIndexOutOfBoundsException)
                thrownExceptions = ArrayIndexOutOfBoundsException(pc) :: thrownExceptions
            if (isNull.maybeYes && throwNullPointerException)
                thrownExceptions = NullPointerException(pc) :: thrownExceptions

            doArraystore(pc, value, index)

            ComputationWithSideEffectOrException(thrownExceptions)
        }

        def getLength(pc: PC): DomainValue =
            IntegerValue(pc)

        def length(pc: PC): Computation[DomainValue, ExceptionValue] = {
            if (isNull == Unknown && throwNullPointerException)
                ComputedValueAndException(getLength(pc), NullPointerException(pc))
            else
                ComputedValue(getLength(pc))
        }
    }

    def asReferenceValue(value: DomainValue): ReferenceValue =
        value.asInstanceOf[ReferenceValue]

    def asObjectValue(value: DomainValue): ObjectValue =
        value.asInstanceOf[ObjectValue]

    def asArrayValue(value: DomainValue): ArrayValue =
        value.asInstanceOf[ArrayValue]

    // -----------------------------------------------------------------------------------
    //
    // QUESTION'S ABOUT VALUES
    //
    // -----------------------------------------------------------------------------------

    override def areEqualReferences(value1: DomainValue, value2: DomainValue): Answer =
        // we could check if it is conceivable that both values are not equal based 
        // on the available type information... However, if we only have a 
        // fragmented/incomplete class hierarchy, the information is most likely of limited
        // value
        Unknown

    final override def isValueSubtypeOf(
        value: DomainValue,
        supertype: ReferenceType): Answer =
        asReferenceValue(value).isValueSubtypeOf(supertype)

    /**
     * Determines the nullness-property of the given value.
     *
     * @param value A value of type `ReferenceValue`.
     */
    final override def isNull(value: DomainValue): Answer =
        asReferenceValue(value).isNull

    object NullValue {
        def unapply(value: NullValue): Boolean = true
    }

    // -----------------------------------------------------------------------------------
    //
    // ARRAY RELATED OPERATIONS
    //
    // -----------------------------------------------------------------------------------

    //
    // CREATE ARRAY
    //

    override def newarray(
        pc: PC,
        count: DomainValue,
        componentType: FieldType): Computation[DomainValue, ExceptionValue] = {
        //ComputedValueAndException(TypedValue(ArrayType(componentType)), TypedValue(ObjectType.NegativeArraySizeException))

        ComputedValue(NewArray(pc, ArrayType(componentType)))
    }

    /**
     * @note The componentType may be (again) an array type.
     */
    override def multianewarray(
        pc: PC,
        counts: List[DomainValue],
        arrayType: ArrayType): Computation[DomainValue, ExceptionValue] = {
        //ComputedValueAndException(TypedValue(arrayType), TypedValue(ObjectType.NegativeArraySizeException))
        ComputedValue(NewArray(pc, arrayType))
    }

    //
    // OPERATIONS ON ARRAYS
    // 

    /**
     * Loads the value stored in the array at the given index or throws an
     * exception (`NullPointerException` or `IndexOutOfBoundsException`).
     *
     * @note It is in general not necessary to override this method. If you need
     *      some special handling refine the `load` method defined by the trait
     *      `ArrayValue`.
     */
    override def arrayload(
        pc: PC,
        index: DomainValue,
        arrayref: DomainValue): ArrayLoadResult = {
        if (isNull(arrayref).yes)
            justThrows(NullPointerException(pc))
        else
            // if the bytecode is valid, the type cast (asArrayValue) is safe
            asArrayValue(arrayref).load(pc, index)
    }

    /**
     * Stores the given value in the array at the given index or throws an exception
     * (`NullPointerException`, `ArrayStoreException` or `IndexOutOfBoundsException`).
     *
     * @note It is in general not necessary to override this method. If you need
     *      some special handling refine the `store` method defined by the trait
     *      `ArrayValue`.
     */
    override def arraystore(
        pc: PC,
        value: DomainValue,
        index: DomainValue,
        arrayref: DomainValue): ArrayStoreResult = {
        if (isNull(arrayref).yes)
            justThrows(NullPointerException(pc))
        else
            // if the bytecode is valid, the type cast (asArrayValue) is safe
            asArrayValue(arrayref).store(pc, value, index)
    }

    /**
     * Returns the array's length or throws a `NullPointerException` if the given
     * reference is `null`.
     *
     * @note It is in general not necessary to override this method. If you need
     *      some special handling refine the trait `ArrayValue`.
     */
    override def arraylength(
        pc: PC,
        arrayref: DomainValue): Computation[DomainValue, ExceptionValue] = {
        if (isNull(arrayref).yes)
            throws(NullPointerException(pc))
        else
            asArrayValue(arrayref).length(pc)
    }

    // -----------------------------------------------------------------------------------
    //
    // ADDITIONAL FACTORY METHODS
    //
    // -----------------------------------------------------------------------------------

    /**
     * Factory method to create a new domain value that represents a newly created
     * array (non-null) with an unknown size that is empty.
     *
     * ==Typical Usage==
     * This factory method is (implicitly) used, e.g., by BATAI when a new array
     * instruction is found.
     *
     * ==Summary==
     * The properties of the value are:
     *
     *  - Type: '''Precise'''
     *  - Null: '''No'''
     *  - Size: '''Unknown'''
     */
    def NewArray(pc: PC, arrayType: ArrayType): DomainValue

    /**
     * Creates a new `DomainValue` that represents an array value with unknown
     * values and where the specified type may also just be an upper type bound
     * (unless the component type is a primitive type or an array of primitives.)
     *
     * ==Typical Usage==
     * This factory method is (typically) used to create a domain value that represents
     * an array if we know nothing specific about the array. E.g., if you want to
     * analyze a method that takes an array as a parameter.
     *
     * ==Summary==
     * The properties of the value are:
     *  - Type: '''Upper Bound'''
     *  - Null: '''Unknown'''
     *  - Size: '''Unknown'''
     *
     * @note Java Arrays are covariant. I.e., `Object[] a = new Serializable[100];`
     *      is valid.
     */
    def ArrayValue(pc: PC, arrayType: ArrayType): DomainValue

    // -----------------------------------------------------------------------------------
    //
    // HANDLING OF CONSTRAINTS
    //
    // -----------------------------------------------------------------------------------

    override def establishUpperBound(
        pc: PC,
        bound: ReferenceType,
        value: DomainValue,
        operands: Operands,
        locals: Locals): (Operands, Locals) = {
        val referenceValue: ReferenceValue = asReferenceValue(value)
        val newReferenceValue = referenceValue.refineUpperTypeBound(pc, bound)
        if (referenceValue eq newReferenceValue)
            (
                operands,
                locals
            )
        else
            (
                operands.map(op ⇒ if (op eq value) newReferenceValue else op),
                locals.map(l ⇒ if (l eq value) newReferenceValue else l)
            )
    }

    protected def updateIsNull(
        pc: PC,
        value: DomainValue,
        isNull: Answer,
        operands: Operands,
        locals: Locals): (Operands, Locals) = {
        val referenceValue: ReferenceValue = asReferenceValue(value)
        val newReferenceValue = referenceValue.updateIsNull(pc, isNull)
        if (referenceValue eq newReferenceValue)
            (
                operands,
                locals
            )
        else
            (
                operands.map(op ⇒ if (op eq value) newReferenceValue else op),
                locals.map(l ⇒ if (l eq value) newReferenceValue else l)
            )
    }

    /**
     * Updates the nullness property (`isNull == No`) of the given value.
     *
     * Calls `updateIsNull` on the given `ReferenceValue` and replaces every occurrence
     * on the stack/in a register with the updated value.
     *
     * @param value A `ReferenceValue`.
     */
    override def establishIsNonNull(
        pc: PC,
        value: DomainValue,
        operands: Operands,
        locals: Locals): (Operands, Locals) =
        updateIsNull(pc, value, No, operands, locals)

    /**
     * Updates the nullness property (`isNull == Yes`) of the given value.
     *
     * Calls `updateIsNull` on the given `ReferenceValue` and replaces every occurrence
     * on the stack/in a register with the updated value.
     *
     * @param value A `ReferenceValue`.
     */
    override def establishIsNull(
        pc: PC,
        value: DomainValue,
        operands: Operands,
        locals: Locals): (Operands, Locals) =
        updateIsNull(pc, value, Yes, operands, locals)
}
/**
 * Defines domain-independent, commonly used upper type bounds.
 *
 * @author Michael Eichberg
 */
object TypeLevelReferenceValues {

    /**
     * Least upper type bound of Java arrays. That is, every Java array
     * is always `Serializable` and `Cloneable`.
     */
    val SerializableAndCloneable: UpperTypeBound =
        UIDList(ObjectType.Serializable, ObjectType.Cloneable)

}
