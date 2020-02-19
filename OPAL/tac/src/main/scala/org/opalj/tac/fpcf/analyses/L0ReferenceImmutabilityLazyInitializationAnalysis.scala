/* BSD 2-Clause License - see OPAL/LICENSE for details. */
package org.opalj.tac.fpcf.analyses

import scala.annotation.switch
import org.opalj.RelationalOperators.EQ
import org.opalj.RelationalOperators.NE
import org.opalj.collection.immutable.IntTrieSet
import org.opalj.br.fpcf.BasicFPCFEagerAnalysisScheduler
import org.opalj.br.fpcf.BasicFPCFLazyAnalysisScheduler
import org.opalj.br.analyses.DeclaredMethods
import org.opalj.br.analyses.DeclaredMethodsKey
import org.opalj.br.analyses.FieldAccessInformationKey
import org.opalj.br.analyses.SomeProject
import org.opalj.br.analyses.cg.ClosedPackagesKey
import org.opalj.br.analyses.cg.TypeExtensibilityKey
import org.opalj.br.cfg.BasicBlock
import org.opalj.br.cfg.CFG
import org.opalj.br.cfg.CFGNode
import org.opalj.br.fpcf.FPCFAnalysisScheduler
import org.opalj.br.fpcf.properties.EscapeProperty
import org.opalj.br.fpcf.properties.FieldPrematurelyRead
import org.opalj.br.fpcf.properties.Purity
import org.opalj.br.fpcf.FPCFAnalysis
import org.opalj.br.fpcf.properties.cg.Callees
import org.opalj.ai.isImmediateVMException
import org.opalj.ai.pcOfImmediateVMException
import org.opalj.ai.pcOfMethodExternalException
import org.opalj.br.ComputationalTypeFloat
import org.opalj.br.ComputationalTypeInt
import org.opalj.br.DeclaredMethod
import org.opalj.br.Field
import org.opalj.br.FloatType
import org.opalj.br.Method
import org.opalj.br.ObjectType
import org.opalj.br.PC
import org.opalj.br.PCs
import org.opalj.br.ReferenceType
import org.opalj.br.fpcf.properties.AtMost
import org.opalj.br.fpcf.properties.EscapeInCallee
import org.opalj.br.fpcf.properties.EscapeViaReturn
import org.opalj.br.fpcf.properties.ImmutableReference
import org.opalj.br.fpcf.properties.LazyInitialization
import org.opalj.br.fpcf.properties.MutableReference
import org.opalj.br.fpcf.properties.NoEscape
import org.opalj.br.fpcf.properties.NoLazyInitialization
import org.opalj.br.fpcf.properties.NotPrematurelyReadField
import org.opalj.br.fpcf.properties.NotThreadSafeLazyInitialization
import org.opalj.br.fpcf.properties.PrematurelyReadField
import org.opalj.br.fpcf.properties.ReferenceImmutability
import org.opalj.br.fpcf.properties.ReferenceImmutabilityLazyInitialization
import org.opalj.fpcf.EOptionP
import org.opalj.fpcf.Entity
import org.opalj.fpcf.FinalEP
import org.opalj.fpcf.FinalP
import org.opalj.fpcf.InterimEP
import org.opalj.fpcf.InterimResult
import org.opalj.fpcf.InterimUBP
import org.opalj.fpcf.LBP
import org.opalj.fpcf.ProperPropertyComputationResult
import org.opalj.fpcf.Property
import org.opalj.fpcf.PropertyBounds
import org.opalj.fpcf.PropertyComputationResult
import org.opalj.fpcf.PropertyStore
import org.opalj.fpcf.SomeEPS
import org.opalj.fpcf.UBP
import org.opalj.fpcf.Result
import org.opalj.tac.Assignment
import org.opalj.tac.CaughtException
import org.opalj.tac.DUVar
import org.opalj.tac.Expr
import org.opalj.tac.FieldWriteAccessStmt
import org.opalj.tac.GetField
import org.opalj.tac.GetStatic
import org.opalj.tac.If
import org.opalj.tac.NonVirtualFunctionCall
import org.opalj.tac.PutField
import org.opalj.tac.PutStatic
import org.opalj.tac.ReturnValue
import org.opalj.tac.StaticFunctionCall
import org.opalj.tac.Stmt
import org.opalj.tac.TACMethodParameter
import org.opalj.tac.TACStmts
import org.opalj.tac.TACode
import org.opalj.tac.VirtualFunctionCall
import org.opalj.tac.common.DefinitionSite
import org.opalj.tac.common.DefinitionSitesKey
import org.opalj.tac.fpcf.properties.TACAI
import org.opalj.value.ValueInformation
import org.opalj.tac.SelfReferenceParameter

import scala.collection.mutable

/**
 *
 *  Implementation is used from the old L2FieldMutability implementation
 *  but the lattice is mapped to the new reference immutability lattice.
 *
 * @note Requires that the 3-address code's expressions are not deeply nested.
 *
 * @author Dominik Helm
 * @author Florian Kübler
 * @author Michael Eichberg
 * @author Tobias Peter Roth
 */
class L0ReferenceImmutabilityLazyInitializationAnalysis private[analyses] (val project: SomeProject)
    extends FPCFAnalysis {

  case class State(
      field: Field,
      var referenceImmutability: ReferenceImmutabilityLazyInitialization = NoLazyInitialization,
      var prematurelyReadDependee: Option[EOptionP[Field, FieldPrematurelyRead]] = None,
      var purityDependees: Set[EOptionP[DeclaredMethod, Purity]] = Set.empty,
      var lazyInitInvocation: Option[(DeclaredMethod, PC)] = None,
      var calleesDependee: Option[EOptionP[DeclaredMethod, Callees]] = None,
      var referenceImmutabilityDependees: Set[EOptionP[Field, ReferenceImmutability]] = Set.empty,
      var escapeDependees: Set[EOptionP[DefinitionSite, EscapeProperty]] = Set.empty,
      var tacDependees: Map[Method, (EOptionP[Method, TACAI], PCs)] = Map.empty,
      var isThreadSafeType: Boolean = true
  ) {
    def hasDependees: Boolean = {
      prematurelyReadDependee.isDefined || purityDependees.nonEmpty ||
      calleesDependee.isDefined || referenceImmutabilityDependees.nonEmpty ||
      escapeDependees.nonEmpty || tacDependees.nonEmpty
    }

    def dependees: Traversable[EOptionP[Entity, Property]] = {
      prematurelyReadDependee ++ purityDependees ++ calleesDependee ++
        referenceImmutabilityDependees ++ escapeDependees ++ tacDependees.valuesIterator.map(_._1)
    }
  }

  type V = DUVar[ValueInformation]

  final val typeExtensibility = project.get(TypeExtensibilityKey)
  final val closedPackages = project.get(ClosedPackagesKey)
  final val fieldAccessInformation = project.get(FieldAccessInformationKey)
  final val definitionSites = project.get(DefinitionSitesKey)
  implicit final val declaredMethods: DeclaredMethods = project.get(DeclaredMethodsKey)

  def doDetermineReferenceImmutability(entity: Entity): PropertyComputationResult = entity match {
    case field: Field => determineReferenceImmutabilityLazyInitialization(field)
    case _ =>
      val m = entity.getClass.getSimpleName + " is not an org.opalj.br.Field"
      throw new IllegalArgumentException(m)
  }

  /**
   * Analyzes the mutability of private non-final fields.
   *
   * This analysis is only ''soundy'' if the class file does not contain native methods.
   * If the analysis is schedulued using its companion object all class files with
   * native methods are filtered.
   */
  private[analyses] def determineReferenceImmutabilityLazyInitialization(
      field: Field
  ): ProperPropertyComputationResult = {
    implicit val state: State = State(field)
    // Fields are not final if they are read prematurely!
    //if (isPrematurelyRead(propertyStore(field, FieldPrematurelyRead.key)))
    //    return Result(field, MutableReference); //Result(field, NonFinalFieldByAnalysis);
    //if (field.isFinal)
    //    return createResult();

    //state.referenceImmutability = ImmutableReference //EffectivelyFinalField

    //val thisType = field.classFile.thisType

    //if (field.isPublic)
    //    return Result(field, MutableReference); //Result(field, NonFinalFieldByLackOfInformation)

    // Collect all classes that have access to the field, i.e. the declaring class and possibly
    // classes in the same package as well as subclasses
    // Give up if the set of classes having access to the field is not closed
    /**
     * val initialClasses =
     * if (field.isProtected || field.isPackagePrivate) {
     * if (!closedPackages.isClosed(thisType.packageName)) {
     * return Result(field, MutableReference); //return Result(field, NonFinalFieldByLackOfInformation);
     * }
     * project.classesPerPackage(thisType.packageName)
     * } else {
     * Set(field.classFile)
     * }
     *
     * val classesHavingAccess: Iterator[ClassFile] =
     * if (field.isProtected) {
     * if (typeExtensibility(thisType).isYesOrUnknown) {
     * return Result(field, MutableReference); //return Result(field, NonFinalFieldByLackOfInformation);
     * }
     * val subclassesIterator: Iterator[ClassFile] =
     * classHierarchy.allSubclassTypes(thisType, reflexive = false).flatMap { ot =>
     * project.classFile(ot).filter(cf => !initialClasses.contains(cf))
     * }
     * initialClasses.iterator ++ subclassesIterator
     * } else {
     * initialClasses.iterator
     * }
     */
    // If there are native methods, we give up
    ///if (classesHavingAccess.exists(_.methods.exists(_.isNative)))
    ///  return Result(field, MutableReference); //return Result(field, NonFinalFieldByLackOfInformation);

    // We now (compared to the simple one) have to analyze the static initializer as
    // the static initializer can be used to initialize a private field of an instance
    // of the class after the reference to the class and (an indirect) reference to the
    // field has become available. Consider the following example:
    // class X implements Y{
    //
    //     private Object o;
    //
    //     public Object getO() { return o; }
    //
    //     private static X instance;
    //     static {
    //         instance = new X();
    //         Z.register(instance);
    //         // when we reach this point o is now (via getO) publically accessible and
    //         // X is properly initialized!
    //         o = new Object(); // o is mutated...
    //     }
    // }

    /**
     * Returns the TACode for a method if available, registering dependencies as necessary.
     */
    def getTACAI(
        method: Method,
        pcs: PCs
    )(implicit state: State): Option[TACode[TACMethodParameter, V]] = {
      propertyStore(method, TACAI.key) match {
        case finalEP: FinalEP[Method, TACAI] =>
          finalEP.ub.tac
        case eps: InterimEP[Method, TACAI] =>
          state.tacDependees += method -> ((eps, pcs))
          eps.ub.tac
        case epk =>
          state.tacDependees += method -> ((epk, pcs))
          None
      }
    }

    for {
      (method, pcs) <- fieldAccessInformation.writeAccesses(field)
      taCode <- getTACAI(method, pcs)
    } {
      //if (
      checkMethod(method, taCode, pcs) //) {
      // return Result(field, MutableReference); //return Result(field, NonFinalFieldByAnalysis);
      //}

    }

    ///if (state.lazyInitInvocation.isDefined) {
    ///  val calleesEOP = propertyStore(state.lazyInitInvocation.get._1, Callees.key)
    ///  handleCalls(calleesEOP)
    ///}

    createResult()
  }

  /**
   * def handleCalls(
   * calleesEOP: EOptionP[DeclaredMethod, Callees]
   * )(
   * implicit
   * state: State
   * ): Boolean = {
   * calleesEOP match {
   * case FinalP(callees) =>
   * state.calleesDependee = None
   * handleCallees(callees)
   * case InterimUBP(callees) =>
   * state.calleesDependee = Some(calleesEOP)
   * handleCallees(callees)
   * case _ =>
   * state.calleesDependee = Some(calleesEOP)
   * false
   * }
   * }*
   */
  /**
   *
   * def handleCallees(callees: Callees)(implicit state: State): Boolean = {
   * val pc = state.lazyInitInvocation.get._2
   * if (callees.isIncompleteCallSite(pc)) {
   * state.referenceImmutability = MutableReference //NonFinalFieldByAnalysis
   * true
   * } else {
   * val targets = callees.callees(pc).toTraversable
   * if (targets.exists(target => isNonDeterministic(propertyStore(target, Purity.key)))) {
   * state.referenceImmutability = MutableReference //NonFinalFieldByAnalysis
   * true
   * } else false
   * }
   * }*
   */
  /**
   * Returns the value the field will have after initialization or None if there may be multiple
   * values.
   */
  def getDefaultValue()(implicit state: State): Option[Any] = {
    Some(if (state.field.fieldType eq FloatType) 0.0f else 0)

    /* TODO Some lazy initialized fields use a different value to mark an uninitialized field
     * The code below can be used to identify such value, but is not yet adapted to using the
     * TACAI property */
    /*
        var constantVal: Option[Any] = None
        var allInitializeConstant = true

        val field = state.field
        var constructors: Set[Method] =
            if(field.isStatic) Set.empty else field.classFile.constructors.toSet

        val writesIterator = fieldAccessInformation.writeAccesses(field).iterator
        while (writesIterator.hasNext && allInitializeConstant) {
            val (method, pc) = writesIterator.next()
            constructors -= method
            val code = tacai(method).stmts

            val index = pcToIndex(pc)
            val stmt = code(index)
            if (stmt.astID == PutStatic.ASTID ||
                stmt.asPutField.objRef.asVar.definedBy == SelfReferenceParameter) {
                val write = stmt.asFieldWriteAccessStmt
                if (write.resolveField(p).contains(state.field)) {
                    val defs = write.value.asVar.definedBy
                    if (defs.size == 1 && defs.head >= 0) {
                        val defSite = code(defs.head).asAssignment.expr
                        val const = if (defSite.isIntConst)
                            Some(defSite.asIntConst.value)
                        else if (defSite.isFloatConst)
                            Some(defSite.asFloatConst.value)
                        else None
                        if (const.isDefined) {
                            if (constantVal.isDefined) {
                                if (constantVal != const) {
                                    allInitializeConstant = false
                                    constantVal = None
                                }
                            } else constantVal = const
                        } else {
                            allInitializeConstant = false
                            constantVal = None
                        }
                    }
                }
            }
        }

        for (constructor ← constructors) {
            // TODO iterate all statements
            val NonVirtualMethodCall(_, declClass, _, name, _, rcvr, _) = stmt
            // Consider calls to other constructors as initializations as either
            // the called constructor will initialize the field or delegate to yet
            // another constructor
            if (declClass != state.field.classFile.thisType || name != "<init>" ||
                rcvr.asVar.definedBy != SelfReferenceParameter) {
                if (constantVal.isDefined) allInitializeConstant = false
                else constantVal = Some(if (state.field.fieldType eq FloatType) 0.0f else 0)
            }
        }

        constantVal */
  }

  /**
   * Prepares the PropertyComputation result, either as IntermediateResult if there are still
   * dependees or as Result otherwise.
   */
  def createResult()(implicit state: State): ProperPropertyComputationResult = {
    if (state.hasDependees && (state.referenceImmutability ne NoLazyInitialization)) //NonFinalFieldByAnalysis))
      InterimResult(
        state.field,
        NoLazyInitialization, //MutableReference, //NonFinalFieldByAnalysis,
        state.referenceImmutability,
        state.dependees,
        c
      )
    else {
      if (state.referenceImmutability == LazyInitialization && !state.isThreadSafeType)
        Result(state.field, NotThreadSafeLazyInitialization)
      else
        Result(state.field, state.referenceImmutability)
    }
  }

  /**
   * Continuation function handling updates to the FieldPrematurelyRead property or to the purity
   * property of the method that initializes a (potentially) lazy initialized field.
   */
  def c(eps: SomeEPS)(implicit state: State): ProperPropertyComputationResult = {
    val isNotFinal: Boolean = eps.pk match {
      case EscapeProperty.key =>
        val newEP = eps.asInstanceOf[EOptionP[DefinitionSite, EscapeProperty]]
        state.escapeDependees = state.escapeDependees.filter(_.e ne newEP.e)
        handleEscapeProperty(newEP)
      case TACAI.key =>
        val newEP = eps.asInstanceOf[EOptionP[Method, TACAI]]
        val method = newEP.e
        val pcs = state.tacDependees(method)._2
        state.tacDependees -= method
        if (eps.isRefinable)
          state.tacDependees += method -> ((newEP, pcs))
        checkMethod(method, newEP.ub.tac.get, pcs)
      //case Callees.key =>
      //  handleCalls(eps.asInstanceOf[EOptionP[DeclaredMethod, Callees]])
      case FieldPrematurelyRead.key =>
        isPrematurelyRead(eps.asInstanceOf[EOptionP[Field, FieldPrematurelyRead]])
      case Purity.key =>
        val newEP = eps.asInstanceOf[EOptionP[DeclaredMethod, Purity]]
        state.purityDependees = state.purityDependees.filter(_.e ne newEP.e)
        isNonDeterministic(newEP)
      case ReferenceImmutabilityLazyInitialization.key =>
        val newEP = eps.asInstanceOf[EOptionP[Field, ReferenceImmutability]]
        state.referenceImmutabilityDependees =
          state.referenceImmutabilityDependees.filter(_.e ne newEP.e)
        !isImmutableReference(newEP)
    }

    if (isNotFinal) {
      Result(state.field, NoLazyInitialization) //Result(state.field, NonFinalFieldByAnalysis)
    } else
      createResult()
  }

  /**
   * Checks whether a field write may be a lazy initialization.
   *
   * We only consider simple cases of lazy initialization where the single field write is guarded
   * so it is executed only if the field still has its default value and where the written value
   * is guaranteed to be the same even if the write is executed multiple times (may happen because
   * of the initializing method being executed more than once on concurrent threads).
   *
   * Also, all field reads must be used only for a guard or their uses have to be guarded
   * themselves.
   */
  def isLazyInitialization(
      writeIndex: Int,
      defaultValue: Any,
      method: Method,
      code: Array[Stmt[V]],
      cfg: CFG[Stmt[V], TACStmts[V]],
      pcToIndex: Array[Int],
      pcs: PCs
  )(implicit state: State): Boolean = {
    //xx println("PS: "+propertyStore(declaredMethods(method), Purity.key))
    val write = code(writeIndex).asFieldWriteAccessStmt
    //xx println("1")
    if (state.field.fieldType.computationalType != ComputationalTypeInt &&
        state.field.fieldType.computationalType != ComputationalTypeFloat) {
      // Only handle lazy initialization of ints and floats as they are guaranteed to be
      // written atomically
      ////return false;
      state.isThreadSafeType = false
    }
    //xx println("2")
    val reads = fieldAccessInformation.readAccesses(state.field)
    if (reads.exists(mAndPCs => (mAndPCs._1 ne method) && !mAndPCs._1.isInitializer)) {
      return false; // Reads outside the (single) lazy initialization method
    }
    //xx println("3")
    // There must be a guarding if-Statement
    // The guardIndex is the index of the if-Statement, the guardedIndex is the index of the
    // first statement that is executed after the if-Statement if the field's value was not the
    // default value
    val (guardIndex, guardedIndex, readIndex) =
      findGuard(writeIndex, defaultValue, code, cfg) match {
        case Some((guard, guarded, read)) => (guard, guarded, read)
        case None                         => return false;
      }
    //xx println("4")
    // Detect only simple patterns where the lazily initialized value is returned immediately
    if (!checkImmediateReturn(write, writeIndex, readIndex, code)) //return false;
      // possibly double checked locking
      {
        //TODO
        if (isDoubleCheckedLocking(method, pcs)) {
          state.isThreadSafeType = true;
          return true;
        } else
          return false;
      }
    //xx println("5")
    // The value written must be computed deterministically and the writes guarded correctly
    if (!checkWrites(write, writeIndex, guardIndex, guardedIndex, method, code, cfg))
      return false;
    //xx println("6")
    // Field reads (except for the guard) may only be executed if the field's value is not the
    // default value
    if (!checkReads(reads, readIndex, guardedIndex, writeIndex, cfg, pcToIndex))
      return false;
    //xx println("7")
    true
  }

  def getTACAI(
      method: Method,
      pcs: PCs
  )(implicit state: State): Option[TACode[TACMethodParameter, V]] = {
    propertyStore(method, TACAI.key) match {
      case finalEP: FinalEP[Method, TACAI] =>
        finalEP.ub.tac
      case eps: InterimEP[Method, TACAI] =>
        state.tacDependees += method -> ((eps, pcs))
        eps.ub.tac
      case epk =>
        state.tacDependees += method -> ((epk, pcs))
        None
    }
  }

  def isDoubleCheckedLocking(method: Method, pcs: PCs)(implicit state: State): Boolean = {
    val fieldAccessInformation = project.get(FieldAccessInformationKey)
    var guards: List[(Int, Int)] = Nil
    var assignments: List[Int] = Nil
    var monitorEnter: Option[(Int, Option[ObjectType])] = None
    var monitorExit: Option[(Int, Option[ObjectType])] = None
    var result = true
    val accessingPcs = fieldAccessInformation.allWriteAccesses
      .filter(p => p._1 == state.field)
      .head
      ._2
      .filter(p => p._1 == method)
      .head
      ._2

    if (state.field.fieldType.isObjectType && method.asMethod.isStatic) {
      val tacCode = getTACAI(method, pcs)
      tacCode match {
        case Some(tac) => {
          val hm = new mutable.HashMap[Int, Assignment[V]]()
          //index for tac-code
          var i: Int = -1
          if (tac != null && tac.instructions != null)
            if (tac != null && tac.instructions != null)
              tac.instructions.foreach(instr => {
                i = i + 1
                if (instr.isIfStmt) {
                  var currentFieldsClassType: ObjectType = null
                  if (state.field.fieldType.isObjectType)
                    currentFieldsClassType = state.field.fieldType.asObjectType // method.classFile.thisType
                  var ifLeftType: ReferenceType = null
                  if (instr.asIf.left.isVar && instr.asIf.left.asVar.value.isReferenceValue) // && instr.asIf.left.asVar.value.asReferenceValue)
                    try {
                      ifLeftType = instr.asIf.left.asVar.value.asReferenceValue.asReferenceType
                    } catch {
                      case _: Throwable =>
                    }

                  if (//is non null check
                      instr.asIf.condition == NE &&
                      // has same type as field
                      currentFieldsClassType != null &&
                      ifLeftType != null &&
                      ifLeftType.equals(currentFieldsClassType) //guards  field?
                      ) {
                    // => guards  the value
                    guards = (i, instr.asIf.target) :: guards
                  }
                }
                if (instr.isAssignment) {
                  hm += (i -> instr.asAssignment)
                  if (accessingPcs.contains(instr.pc))
                    assignments = instr.pc.toInt :: assignments
                }

                if (instr.isPutStatic && accessingPcs.contains(instr.pc)) {
                  assignments = i :: assignments
                }

                if (instr.isMonitorEnter) {
                  val defB = instr.asMonitorEnter.objRef.asVar.definedBy.head
                  var objTypeOfMonitorEnter: Option[ObjectType] = None
                  try {
                    objTypeOfMonitorEnter =
                      Some(hm(defB).asAssignment.expr.asClassConst.value.asObjectType)
                  } catch {
                    case _: Throwable =>
                  }
                  monitorEnter = Some((i, objTypeOfMonitorEnter))
                  //hm.foreach(x ⇒ println(x))
                }

                if (instr.isMonitorExit) {
                  val defB = instr.asMonitorExit.objRef.asVar.definedBy.head
                  var objTypeOfMonitorExit: Option[ObjectType] = None
                  try {
                    objTypeOfMonitorExit =
                      Some(hm(defB).asAssignment.expr.asClassConst.value.asObjectType)
                  } catch {
                    case _: Throwable =>
                  }
                  monitorExit = Some((i, objTypeOfMonitorExit))
                }
              })
        }
        case _ => result = false
      }
      assignments.foreach(a => guards.foreach(g => result = result && a > g._1 && a < g._2))
      result = result && assignments.size >= 1
      result = result && monitorEnter != None && monitorExit != None
      monitorEnter match {
        case Some((n, Some(ot: ObjectType))) =>
          result = result &&
            ot == state.field.fieldType.asObjectType &&
            //outer guard(s)
            guards.filter(x => n > x._1).size >= 1 &&
            //inner guard(s)
            guards.filter(x => n < x._1).size >= 1
        case None => result = false
        case _    => result = false
      }
      monitorExit match {
        case Some((n, Some(ot: ObjectType))) if (ot == state.field.fieldType.asObjectType) =>
        case None                                                                          => result = false
        case _                                                                             => result = false
      }
      result
    } else
      false
  }

  def checkWrites(
      write: FieldWriteAccessStmt[V],
      writeIndex: Int,
      guardIndex: Int,
      guardedIndex: Int,
      method: Method,
      code: Array[Stmt[V]],
      cfg: CFG[Stmt[V], TACStmts[V]]
  )(implicit state: State): Boolean = {
    //xx println("51")
    val definitions = write.value.asVar.definedBy
    //xx println("52")
    val isDeterministic =
      if (definitions.size == 1) {
        // The value written must be computed deterministically
        //xx println("53")
        checkWriteIsDeterministic(code(definitions.head).asAssignment, method, code)
      } else {
        // More than one definition site for the value might lead to differences between
        // invocations, but not if this method has no parameters and is deterministic
        // (in this case, the definition reaching the write will always be the same)
        //xx println("54")
        method.descriptor.parametersCount == 0 &&
        !isNonDeterministic(propertyStore(declaredMethods(method), Purity.key))
      }
    //xx println("55")
    // The field write must be guarded correctly
    val r1 = isDeterministic
    val r2 = checkWriteIsGuarded(writeIndex, guardIndex, guardedIndex, method, code, cfg)
    //xx println("isDeterministic: "+r1)
    //xx println("checkWriteIsGuarded: "+r2)
    //r1 && r2 //TODO check
    if (!(r1 && r2) && !state.isThreadSafeType) {
      //state.isThreadSafeType = false
      true
    } else
      r1 && r2
  }

  /**
   * Checks if the field write for a lazy initialization is immediately followed by a return of
   * the written value (possibly loaded by another field read).
   */
  def checkImmediateReturn(
      write: FieldWriteAccessStmt[V],
      writeIndex: Int,
      readIndex: Int,
      code: Array[Stmt[V]]
  )(implicit state: State): Boolean = {
    var index = writeIndex + 1

    var load = -1

    while (index < code.length) {
      val stmt = code(index)
      stmt.astID match {
        case Assignment.ASTID =>
          if (isReadOfCurrentField(stmt.asAssignment.expr))
            load = index
          else
            return false; // No field read or a field read of a different field
        case ReturnValue.ASTID =>
          val returnValueDefs = stmt.asReturnValue.expr.asVar.definedBy
          if (returnValueDefs.size == 2 &&
              returnValueDefs.contains(write.value.asVar.definedBy.head) &&
              returnValueDefs.contains(readIndex))
            return true; // direct return of the written value
          else if (load >= 0 && (returnValueDefs == IntTrieSet(load) ||
                   returnValueDefs == IntTrieSet(readIndex, load)))
            return true; // return of field value loaded by field read
          else
            return false; // return of different value
        case _ => return false; // neither a field read nor a return
      }
      index += 1
    }
    false
  }

  def lazyInitializerIsDeterministic(
      method: Method,
      code: Array[Stmt[V]]
  )(implicit state: State): Boolean = {
    val result = (method.descriptor.parametersCount == 0 && !isNonDeterministic(
      propertyStore(declaredMethods(method), Purity.key)
    ))
    result
  }

  /**
   * Analyzes field writes for a single method, returning false if the field may still be
   * effectively final and true otherwise.
   */
  def checkMethod(
      method: Method,
      taCode: TACode[TACMethodParameter, V],
      pcs: PCs
  )(implicit state: State): Boolean = {
    val field = state.field
    val stmts = taCode.stmts
    for (pc <- pcs) {
      val index = taCode.pcToIndex(pc)
      if (index >= 0) {
        val stmt = stmts(index)
        if (stmt.pc == pc) {
          stmt.astID match {
            case PutStatic.ASTID | PutField.ASTID =>
              if (method.isInitializer) {

                if (field.isStatic) {
                  if (method.isConstructor)
                    return true;
                } else {
                  val receiverDefs = stmt.asPutField.objRef.asVar.definedBy
                  if (receiverDefs != SelfReferenceParameter)
                    return true;
                }
              } else {
                if (field.isStatic ||
                    stmt.asPutField.objRef.asVar.definedBy == SelfReferenceParameter) {
                  // We consider lazy initialization if there is only single write
                  // outside an initializer, so we can ignore synchronization
                  if (state.referenceImmutability == NotThreadSafeLazyInitialization || state.referenceImmutability == LazyInitialization) //LazyInitializedField)
                    return true;

                  // A lazily initialized instance field must be initialized only
                  // by its owning instance
                  if (!field.isStatic &&
                      stmt.asPutField.objRef.asVar.definedBy != SelfReferenceParameter)
                    return true;

                  val defaultValue = getDefaultValue()
                  if (defaultValue.isEmpty)
                    return true;

                  // A field written outside an initializer must be lazily
                  // initialized or it is non-final
                  if (!isLazyInitialization(
                        index,
                        defaultValue.get,
                        method,
                        taCode.stmts,
                        taCode.cfg,
                        taCode.pcToIndex,
                        pcs
                      ))
                    return true;
                  state.referenceImmutability = LazyInitialization //LazyInitializedField
                } else if (referenceHasEscaped(stmt.asPutField.objRef.asVar, stmts, method)) {
                  // note that here we assume real three address code (flat hierarchy)

                  // for instance fields it is okay if they are written in the
                  // constructor (w.r.t. the currently initialized object!)

                  // If the field that is written is not the one referred to by the
                  // self reference, it is not effectively final.

                  // However, a method (e.g. clone) may instantiate a new object and
                  // write the field as long as that new object did not yet escape.
                  return true;
                }
              }
            case _ => throw new RuntimeException("unexpected field access");
          }
        } else {
          // nothing to do as the put field is dead
        }
      }
    }
    false
  }

  /**
   * def getTACAI(
   * method: Method,
   * pcs:    PCs
   * )(implicit state: State): Option[TACode[TACMethodParameter, V]] = {
   * propertyStore(method, TACAI.key) match {
   * case finalEP: FinalEP[Method, TACAI] ⇒
   * finalEP.ub.tac
   * case eps: InterimEP[Method, TACAI] ⇒
   * state.tacDependees += method → ((eps, pcs))
   * eps.ub.tac
   * case epk ⇒
   * state.tacDependees += method → ((epk, pcs))
   * None
   * }
   * }
   * Returns the TACode for a method if available, registering dependencies as necessary.
   */
  /**
   * Checks whether the object reference of a PutField does escape (except for being returned).
   */
  def referenceHasEscaped(
      ref: V,
      stmts: Array[Stmt[V]],
      method: Method
  )(implicit state: State): Boolean = {
    ref.definedBy.forall { defSite =>
      if (defSite < 0) true // Must be locally created
      else {
        val definition = stmts(defSite).asAssignment
        // Must either be null or freshly allocated
        if (definition.expr.isNullExpr) false
        else if (!definition.expr.isNew) true
        else {
          val escape =
            propertyStore(definitionSites(method, definition.pc), EscapeProperty.key)
          handleEscapeProperty(escape)
        }
      }
    }
  }

  /**
   * Handles the influence of an escape property on the field mutability.
   * @return true if the object - on which a field write occurred - escapes, false otherwise.
   * @note (Re-)Adds dependees as necessary.
   */
  def handleEscapeProperty(
      ep: EOptionP[DefinitionSite, EscapeProperty]
  )(implicit state: State): Boolean = ep match {
    case FinalP(NoEscape | EscapeInCallee | EscapeViaReturn) =>
      false

    case FinalP(AtMost(_)) =>
      true

    case _: FinalEP[DefinitionSite, EscapeProperty] =>
      true // Escape state is worse than via return

    case InterimUBP(NoEscape | EscapeInCallee | EscapeViaReturn) =>
      state.escapeDependees += ep
      false

    case InterimUBP(AtMost(_)) =>
      true

    case _: InterimEP[DefinitionSite, EscapeProperty] =>
      true // Escape state is worse than via return

    case _ =>
      state.escapeDependees += ep
      false
  }

  /**
   * Checks if the field write is only executed if the field's value was still the default value.
   * Also, no exceptions may be thrown between the guarding if-Statement of a lazy initialization
   * and the field write.
   */
  def checkWriteIsGuarded(
      writeIndex: Int,
      guardIndex: Int,
      guardedIndex: Int,
      method: Method,
      code: Array[Stmt[V]],
      cfg: CFG[Stmt[V], TACStmts[V]]
  )(implicit state: State): Boolean = {
    val startBB = cfg.bb(writeIndex).asBasicBlock

    var enqueuedBBs: Set[CFGNode] = Set(startBB)
    var worklist: List[BasicBlock] = List(startBB.asBasicBlock)

    val abnormalReturnNode = cfg.abnormalReturnNode
    val caughtExceptions = code filter { stmt =>
      stmt.astID == CaughtException.ASTID
    } flatMap { exception =>
      exception.asCaughtException.origins.map { origin: Int =>
        if (isImmediateVMException(origin))
          pcOfImmediateVMException(origin)
        else
          pcOfMethodExternalException(origin)
      }.iterator
    }

    while (worklist.nonEmpty) {
      val curBB = worklist.head
      worklist = worklist.tail

      val startPC = curBB.startPC
      val endPC = curBB.endPC

      if (startPC == 0 || startPC == guardedIndex)
        return false; // Reached method start or wrong branch of guarding if-Statement

      // Exception thrown between guard and write, which is ok for deterministic methods,
      // but may be a problem otherwise as the initialization is not guaranteed to happen
      // (or never happen).
      if ((curBB ne startBB) && abnormalReturnNode.predecessors.contains(curBB))
        if (!lazyInitializerIsDeterministic(method, code))
          return false;

      // Exception thrown between guard and write (caught somewhere, but we don't care)
      if ((curBB ne startBB) & caughtExceptions.contains(endPC))
        if (!lazyInitializerIsDeterministic(method, code))
          return false;

      // Check all predecessors except for the one that contains the guarding if-Statement
      val predecessors = getPredecessors(curBB, enqueuedBBs).filterNot(_.endPC == guardIndex)
      worklist ++= predecessors
      enqueuedBBs ++= predecessors
    }

    true
  }

  /**
   * Checks if the value written to the field is guaranteed to be always the same.
   * This is true if the value is constant or originates from a deterministic call of a method
   * without non-constant parameters. Alternatively, if the initialization method itself is
   * deterministic and has no parameters, the value is also always the same.
   */
  def checkWriteIsDeterministic(
      origin: Assignment[V],
      method: Method,
      code: Array[Stmt[V]]
  )(implicit state: State): Boolean = {
    //xx println("begin checkwriteisdeterministic")
    def isConstant(uvar: Expr[V]): Boolean = {
      val defSites = uvar.asVar.definedBy

      def isConstantDef(index: Int) = {
        //xx println("index: "+index)
        if (index < 0) false
        else if (code(defSites.head).asAssignment.expr.isConst) true
        else {
          val expr = code(index).asAssignment.expr
          //xx println("expr: "+expr)
          //println("resolveField(p): " + expr.asFieldRead.resolveField(p))
          expr.isFieldRead && (expr.asFieldRead.resolveField(p) match {
            case Some(field) =>
              isImmutableReference(propertyStore(field, ReferenceImmutability.key))
            case _ => // Unknown field
              false
          })
        }
      }

      defSites == SelfReferenceParameter ||
      defSites.size == 1 && isConstantDef(defSites.head)
    }

    val value = origin.expr

    //xx println("value.astID: "+value.astID)

    val isNonConstDeterministic = value.astID match {
      case GetStatic.ASTID | GetField.ASTID =>
        //xx println("a")
        value.asFieldRead.resolveField(p) match {
          case Some(field) =>
            //xx println("b")
            isImmutableReference(propertyStore(field, ReferenceImmutability.key))
          case _ => // Unknown field
            //xx println("c")
            false
        }
      case StaticFunctionCall.ASTID | NonVirtualFunctionCall.ASTID | VirtualFunctionCall.ASTID =>
        //xx println("d")
        // If the value originates from a call, that call must be deterministic and may not
        // have any non constant parameters to guarantee that it is the same on every
        // invocation. The receiver object must be the 'this' self reference for the same
        // reason.
        if (value.asFunctionCall.allParams.exists(!isConstant(_))) {
          //xx println("e")
          false
        } else {
          //xx println("f")
          state.lazyInitInvocation = Some((declaredMethods(method), origin.pc))
          true
        }
      case _ =>
        //xx println("g")
        // The value neither is a constant nor originates from a call, but if the
        // current method does not take parameters and is deterministic, the value is
        // guaranteed to be the same on every invocation.
        lazyInitializerIsDeterministic(method, code)
    }

    value.isConst || isNonConstDeterministic
  }

  /**
   * Checks that all non-dead field reads that are not used for the guarding if-Statement of a
   * lazy initialization are only executed if the field did not have its default value or after
   * the (single) field write.
   */
  def checkReads(
      reads: Seq[(Method, PCs)],
      readIndex: Int,
      guardedIndex: Int,
      writeIndex: Int,
      cfg: CFG[Stmt[V], TACStmts[V]],
      pcToIndex: Array[Int]
  ): Boolean = {
    // There is only a single method with reads aside from initializers (checked by
    // isLazilyInitialized), so we have to check only reads from that one method.
    reads.filter(!_._1.isInitializer).head._2 forall { readPC =>
      val index = pcToIndex(readPC)
      index != -1 || index == readIndex || checkRead(index, guardedIndex, writeIndex, cfg)
    }
  }

  /**
   * Checks that a field read is only executed if the field did not have its default value or
   * after the (single) field write.
   */
  def checkRead(
      readIndex: Int,
      guardedIndex: Int,
      writeIndex: Int,
      cfg: CFG[Stmt[V], TACStmts[V]]
  ): Boolean = {
    val startBB = cfg.bb(readIndex).asBasicBlock
    val writeBB = cfg.bb(writeIndex)

    var enqueuedBBs: Set[CFGNode] = Set(startBB)
    var worklist: List[BasicBlock] = List(startBB.asBasicBlock)

    while (worklist.nonEmpty) {
      val curBB = worklist.head
      worklist = worklist.tail

      val startPC = curBB.startPC

      if (startPC == 0)
        return false; // Reached the start of the method but not the guard or field write

      if ((curBB eq writeBB) && writeIndex > readIndex)
        return false; // In the basic block of the write, but before the write

      if (startPC != guardedIndex && // Did not reach the guard
          (curBB ne writeBB) /* Not the basic block of the write */ ) {
        val predecessors = getPredecessors(curBB, enqueuedBBs)
        worklist ++= predecessors
        enqueuedBBs ++= predecessors
      }
    }

    true
  }

  /**
   * Gets all predecessor BasicBlocks of a CFGNode.
   */
  def getPredecessors(node: CFGNode, visited: Set[CFGNode]): List[BasicBlock] = {
    val result = node.predecessors.iterator flatMap { curNode =>
      if (curNode.isBasicBlock)
        if (visited.contains(curNode)) None
        else Some(curNode.asBasicBlock)
      else getPredecessors(curNode, visited)
    }
    result.toList
  }

  /**
   * Finds the index of the guarding if-Statement for a lazy initialization, the index of the
   * first statement executed if the field does not have its default value and the index of the
   * field read used for the guard.
   */
  def findGuard(
      fieldWrite: Int,
      defaultValue: Any,
      code: Array[Stmt[V]],
      cfg: CFG[Stmt[V], TACStmts[V]]
  )(implicit state: State): Option[(Int, Int, Int)] = {
    val startBB = cfg.bb(fieldWrite).asBasicBlock

    var enqueuedBBs: Set[CFGNode] = startBB.predecessors
    var worklist: List[BasicBlock] = getPredecessors(startBB, Set.empty)

    var result: Option[(Int, Int)] = None

    while (worklist.nonEmpty) {
      val curBB = worklist.head
      worklist = worklist.tail

      val startPC = curBB.startPC
      val endPC = curBB.endPC

      val cfStmt = code(endPC)
      (cfStmt.astID: @switch) match {
        case If.ASTID =>
          val ifStmt = cfStmt.asIf
          ifStmt.condition match {
            case EQ if curBB != startBB && isGuard(ifStmt, defaultValue, code) =>
              if (result.isDefined) {
                if (result.get._1 != endPC || result.get._2 != endPC + 1)
                  return None;
              } else {
                result = Some((endPC, endPC + 1))
              }

            case NE if curBB != startBB && isGuard(ifStmt, defaultValue, code) =>
              if (result.isDefined) {
                if (result.get._1 != endPC || result.get._2 != ifStmt.targetStmt)
                  return None;
              } else {
                result = Some((endPC, ifStmt.targetStmt))
              }

            // Otherwise, we have to ensure that a guard is present for all predecessors
            case _ =>
              if (startPC == 0) return None;

              val predecessors = getPredecessors(curBB, enqueuedBBs)
              worklist ++= predecessors
              enqueuedBBs ++= predecessors
          }

        // Otherwise, we have to ensure that a guard is present for all predecessors
        case _ =>
          if (startPC == 0) return None;

          val predecessors = getPredecessors(curBB, enqueuedBBs)
          worklist ++= predecessors
          enqueuedBBs ++= predecessors
      }
    }

    if (result.isDefined) {
      // The field read that defines the value checked by the guard must be used only for the
      // guard or directly if the field's value was not the default value
      val ifStmt = code(result.get._1).asIf
      val expr = if (ifStmt.leftExpr.isConst) ifStmt.rightExpr else ifStmt.leftExpr
      val definitions = expr.asVar.definedBy
      var fieldReadUses: IntTrieSet = IntTrieSet.empty
      if (definitions.head >= 0 && code(definitions.head).isAssignment)
        fieldReadUses = code(definitions.head).asAssignment.targetVar.usedBy
      val fieldReadUsedCorrectly = fieldReadUses forall { use =>
        use == result.get._1 || use == result.get._2
      }
      if (definitions.size == 1 && fieldReadUsedCorrectly)
        return Some((result.get._1, result.get._2, definitions.head)); // Found proper guard
    }

    None
  }

  /**
   * Checks if an expression is a field read of the currently analyzed field.
   * For instance fields, the read must be on the `this` reference.
   */
  def isReadOfCurrentField(expr: Expr[V])(implicit state: State): Boolean = {
    val field = expr.astID match {
      case GetField.ASTID =>
        val objRefDefinition = expr.asGetField.objRef.asVar.definedBy
        if (objRefDefinition != SelfReferenceParameter) None
        else expr.asGetField.resolveField(project)
      case GetStatic.ASTID => expr.asGetStatic.resolveField(project)
      case _               => None
    }
    field.contains(state.field)
  }

  /**
   * Determines if an if-Statement is actually a guard for the current field, i.e. it compares
   * the current field to the default value.
   */
  def isGuard(
      ifStmt: If[V],
      defaultValue: Any,
      code: Array[Stmt[V]]
  )(implicit state: State): Boolean = {

    /**
     * Checks if an expression is an IntConst or FloatConst with the corresponding default value.
     */
    def isDefaultConst(expr: Expr[V]): Boolean = {
      //xx println("Expression: "+expr)
      if (expr.isNullExpr)
        true //--
      else if (expr.isVar) {
        val defSites = expr.asVar.definedBy
        val head = defSites.head
        defSites.size == 1 && head >= 0 && isDefaultConst(code(head).asAssignment.expr)
      } else {
        expr.isIntConst && defaultValue == expr.asIntConst.value ||
        expr.isFloatConst && defaultValue == expr.asFloatConst.value
      }
    }

    /**
     * Checks whether the non-constant expression of the if-Statement is a read of the current
     * field.
     */
    def isGuardInternal(expr: V): Boolean = {
      expr.definedBy forall { index =>
        if (index < 0) false // If the value is from a parameter, this can not be the guard
        else isReadOfCurrentField(code(index).asAssignment.expr)
      }
    }

    if (isDefaultConst(ifStmt.leftExpr) && ifStmt.rightExpr.isVar) {
      isGuardInternal(ifStmt.rightExpr.asVar)
    } else if (isDefaultConst(ifStmt.rightExpr) && ifStmt.leftExpr.isVar) {
      isGuardInternal(ifStmt.leftExpr.asVar)
    } else //TODO check
      //false
      {

        //TODO if expression = Nullexpr
        //xx println("----------------------------------------<<<<")
        state.isThreadSafeType = false
        true //TODO check
      }
  }

  /**
   * Checks if the field is prematurely read, i.e. read before it is initialized in the
   * constructor, using the corresponding property.
   */
  def isPrematurelyRead(
      eop: EOptionP[Field, FieldPrematurelyRead]
  )(implicit state: State): Boolean =
    eop match {
      case LBP(NotPrematurelyReadField) =>
        state.prematurelyReadDependee = None
        false
      case UBP(PrematurelyReadField) => true
      case eps =>
        state.prematurelyReadDependee = Some(eps)
        false
    }

  /**
   * Checkes if the method that defines the value assigned to a (potentially) lazily initialized
   * field is deterministic, ensuring that the same value is written even for concurrent
   * executions.
   */
  def isNonDeterministic(
      eop: EOptionP[DeclaredMethod, Purity]
  )(implicit state: State): Boolean = eop match {
    case LBP(p: Purity) if p.isDeterministic  => false
    case UBP(p: Purity) if !p.isDeterministic => true
    case _ =>
      state.purityDependees += eop
      false
  }

  /**
   * Checkes if the field the value assigned to a (potentially) lazily initialized field is final,
   * ensuring that the same value is written even for concurrent executions.
   */
  def isImmutableReference(
      eop: EOptionP[Field, ReferenceImmutability]
  )(implicit state: State): Boolean = eop match {
    case FinalEP(e, ImmutableReference) => true
    case FinalEP(e, MutableReference)   => false
    //
    case LBP(ImmutableReference) => true
    case UBP(MutableReference)   => false

    /**
     * case LBP(_: ImmutableReference) ⇒ //FinalField) ⇒
     * true
     * case UBP(_: MutableReference) ⇒ false // NonFinalField) ⇒ false *
     */
    case _ =>
      state.referenceImmutabilityDependees += eop
      true
  }
}

trait L0ReferenceImmutabilityLazyInitializationAnalysisScheduler extends FPCFAnalysisScheduler {

  final override def uses: Set[PropertyBounds] = Set(
    PropertyBounds.lub(Purity),
    PropertyBounds.lub(FieldPrematurelyRead),
    PropertyBounds.ub(TACAI),
    PropertyBounds.lub(ReferenceImmutabilityLazyInitialization),
    PropertyBounds.lub(ReferenceImmutability),
    PropertyBounds.ub(EscapeProperty)
  )

  final def derivedProperty: PropertyBounds =
    PropertyBounds.lub(ReferenceImmutabilityLazyInitialization)

}

/**
 * Executor for the field mutability analysis.
 */
object EagerL0ReferenceImmutabilityLazyInitializationAnalysis
    extends L0ReferenceImmutabilityLazyInitializationAnalysisScheduler
    with BasicFPCFEagerAnalysisScheduler {

  final override def start(p: SomeProject, ps: PropertyStore, unused: Null): FPCFAnalysis = {
    val analysis = new L0ReferenceImmutabilityLazyInitializationAnalysis(p)
    val fields = p.allFields
    ps.scheduleEagerComputationsForEntities(fields)(
      analysis.determineReferenceImmutabilityLazyInitialization
    )
    analysis
  }

  override def derivesEagerly: Set[PropertyBounds] = Set(derivedProperty)

  override def derivesCollaboratively: Set[PropertyBounds] = Set.empty
}

/**
 * Executor for the lazy field mutability analysis.
 */
object LazyL0ReferenceImmutabilityLazyInitializationAnalysis
    extends L0ReferenceImmutabilityLazyInitializationAnalysisScheduler
    with BasicFPCFLazyAnalysisScheduler {

  final override def register(
      p: SomeProject,
      ps: PropertyStore,
      unused: Null
  ): FPCFAnalysis = {
    val analysis = new L0ReferenceImmutabilityLazyInitializationAnalysis(p)
    ps.registerLazyPropertyComputation(
      ReferenceImmutabilityLazyInitialization.key,
      analysis.determineReferenceImmutabilityLazyInitialization
    )
    analysis
  }

  override def derivesLazily: Some[PropertyBounds] = Some(derivedProperty)
}
