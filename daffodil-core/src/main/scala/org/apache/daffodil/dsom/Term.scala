/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.daffodil.dsom

import java.util.UUID
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.processors.TermRuntimeData
import org.apache.daffodil.grammar.TermGrammarMixin
import org.apache.daffodil.schema.annotation.props.gen.YesNo
import java.lang.{ Integer => JInt }
import org.apache.daffodil.schema.annotation.props.Found
import org.apache.daffodil.schema.annotation.props.gen.NilKind
import org.apache.daffodil.util.ListUtils
import org.apache.daffodil.processors.unparsers.NeverZeroLengthDetector
import org.apache.daffodil.processors.unparsers.PossiblyZeroArrayOccurrencesDetector
import org.apache.daffodil.processors.unparsers.PossiblyZeroLengthModelGroupDetector
import org.apache.daffodil.processors.unparsers.HexBinaryZeroLengthDetector
import org.apache.daffodil.processors.unparsers.NillableStringZeroLengthDetector
import org.apache.daffodil.processors.unparsers.NillableZeroLengthDetector
import org.apache.daffodil.processors.unparsers.NillableHexBinaryZeroLengthDetector
import org.apache.daffodil.processors.unparsers.StringZeroLengthDetector
import org.apache.daffodil.processors.unparsers.NotRepresentedZeroLengthDetector
import org.apache.daffodil.processors.unparsers.ZeroLengthDetector
import org.apache.daffodil.schema.annotation.props.gen.LengthKind
import org.apache.daffodil.schema.annotation.props.gen.Representation
import org.apache.daffodil.dpath.NodeInfo
import org.apache.daffodil.schema.annotation.props.gen.OccursCountKind
import org.apache.daffodil.schema.annotation.props.SeparatorSuppressionPolicy
import org.apache.daffodil.api.WarnID
import org.apache.daffodil.infoset.PartialNextElementResolver

/**
 * Mixin for objects that are shared, but have consistency checks to be run
 * that are based on the concrete Term objects they are associated with.
 *
 * E.g., DFDL statements may have checks that need to know the encoding
 * (if it is known at compile time). We call this on each statement to enable
 * the checking code to be expressed on that statement where it is relevant,
 * but have it be callable from the concrete Term once it is created.
 *
 * This is a way to avoid use of backpointers from shared objects to every
 * thing referencing them.
 */
trait HasTermCheck {

  /**
   * Perform checking of an object against the supplied Term arg.
   */
  final def checkTerm(term: Term): Unit = {
    //
    // This public method calling a protected method lets us play tricks
    // in the future to avoid repeated check calls by memoizing the
    // results.
    //
    check(term)
  }

  /**
   * Override to perform necessary checks that require information about the
   * concrete Term.
   *
   * This avoids the need for the checking code to have a backpointer to the
   * Term.
   */
  protected def check(term: Term): Unit = {
    // by default this does nothing.
  }
}

/**
 * Term, and what is and isn't a Term, is a key concept in DSOM.
 *
 * From elements, ElementRef and LocalElementDecl are Term. A GlobalElementDecl is *not* a Term.
 * From sequences, Sequence and SequenceGroupRef are Term. GlobalSequenceGroupDef is *not* a Term.
 * From choices, Choice and ChoiceGroupRef are Term. GlobalChoiceGroupDef is *not* a Term.
 *
 * Terms are the things we actually generate parsers/unparsers for. Non-Terms just
 * contribute information used by Terms.
 */
trait Term
  extends AnnotatedSchemaComponent
  with ResolvesScopedProperties
  with ResolvesDFDLStatementMixin
  with TermRuntimeValuedPropertiesMixin
  with TermGrammarMixin
  with DelimitedRuntimeValuedPropertiesMixin
  with InitiatedTerminatedTermMixin
  with TermEncodingMixin
  with EscapeSchemeRefMixin {

  requiredEvaluations(annotationObjs)
  requiredEvaluations(nonDefaultPropertySources)
  requiredEvaluations(defaultPropertySources)
  requiredEvaluations(termChecks)

  private lazy val termChecks = {
    statements.foreach { _.checkTerm(this) }
  }

  /**
   * Used to recursively go through Terms and look for DFDL properties that
   * have not been accessed and record it as a warning. This function uses the
   * property cache state to determine which properties have been access, so
   * this function must only be called after all property accesses are complete
   * (e.g. schema compilation has finished) to ensure there are no false
   * positives.
   */
  final lazy val checkUnusedProperties: Unit = {
    // Get the properties defined on this term and what it refers to
    val localProps = formatAnnotation.justThisOneProperties
    val refProps = optReferredToComponent.map { _.formatAnnotation.justThisOneProperties }.getOrElse(Map.empty)

    // If a term references a global simple type, we need to inspect the
    // propCache of the simple type in addition to this terms propCache. This
    // is because some property lookup results are cached on the global simple
    // type, like in the case of type calc properties
    val optSimpleTypeCached = optReferredToComponent.collect { case gstd: GlobalSimpleTypeDef => gstd.propCache }
    val usedProperties = propCache ++ optSimpleTypeCached.getOrElse(Map.empty)

    localProps.foreach {
      case (prop, (value, _)) =>
        if (!usedProperties.contains(prop)) {
          SDW(WarnID.IgnoreDFDLProperty, "DFDL property was ignored: %s=\"%s\"", prop, value)
        }
    }

    refProps.foreach {
      case (prop, (value, _)) =>
        if (!usedProperties.contains(prop)) {
          optReferredToComponent.get.SDW(WarnID.IgnoreDFDLProperty, "DFDL property was ignored: %s=\"%s\"", prop, value)
        }
    }

    termChildren.foreach { _.checkUnusedProperties }
  }

  def optIgnoreCase: Option[YesNo] = {
    val ic = findPropertyOption("ignoreCase")
    ic match {
      case Found(value, location, _, _) => Some(YesNo(value, location))
      case _ => None
    }
  }

  /**
   * A scalar means has no dimension. Exactly one occurrence.
   *
   * Since terms include both model groups and elements, in DFDL v1.0,
   * model groups are always scalar, as DFDL v1.0 doesn't allow min/max
   * occurs on model groups.
   */
  def isScalar: Boolean

  /**
   * Determines if the element is optional, as in has zero or one instance only.
   *
   * There are two senses of optional
   *
   * 1) Optional as in "might not be present" but for any reason.
   * Consistent with this is Required meaning must occur but for any
   * reason. So all the occurrences of an array that has fixed number of
   * occurrences are required, and some of the occurrences of an array
   * that has a variable number of occurrences are optional.
   *
   * 2) Optional is in minOccurs="0" maxOccurs="1".
   *
   * Consistent with (2) is defining array as maxOccurs >= 2, and Required
   * as minOccurs=maxOccurs=1, but there are also special cases for occursCountKind parsed and stopValue
   * since they don't examine min/max occurs - they are only used for validation
   * in those occursCountKinds.
   *
   * The DFDL spec is not entirely consistent here either I don't believe.
   */
  def isOptional: Boolean

  /**
   * An array can have more than 1 occurrence.
   *
   * An optional element (minOccurs=0, maxOccurs=1) is an array only
   * if occursCountKind is parsed, because then the max/min are ignored.
   */
  def isArray: Boolean

  def elementChildren: Seq[ElementBase]

  /**
   * An integer which is the alignment of this term. This takes into account the
   * representation, type, charset encoding and alignment-related properties.
   */
  def alignmentValueInBits: JInt

  /**
   * True if this term is known to have some text aspect. This can be the value, or it can be
   * delimiters.
   * <p>
   * False only if this term cannot ever have text in it. Example: a sequence with no delimiters.
   * Example: a binary int with no delimiters.
   * <p>
   * Note: this is not recursive - it does not roll-up from children terms.
   * TODO: it does have to deal with the prefix length situation. The type of the prefix
   * may be textual.
   * <p>
   * Override in element base to take simple type or prefix length situations into account
   */
  lazy val couldHaveText = hasDelimiters

  //TODO: if we add recursive types capability to DFDL this will have to change
  // but so will many of these compiler passes up and down through the DSOM objects.

  /**
   * The termChildren are the children that are Terms, i.e., derived from the Term
   * base class. This is to make it clear
   * we're not talking about the XML structures inside the XML parent (which might
   * include annotations, etc.
   *
   * For elements this is Nil for simple types, a single model group for
   * complex types. For model groups there can be more children.
   */
  def termChildren: Seq[Term]

  final val tID = UUID.randomUUID()

  /** Overridden as false for elements with dfdl:inputValueCalc property. */
  lazy val isRepresented = true

  /**
   * Does this term have always have statically required instances in the data stream.
   *
   * This excludes elements that have no representation e.g., elements with dfdl:inputValueCalc.
   *
   * Terms that are optional either via element having zero occurrences, or via a choice branch
   * fail this test.
   */
  def hasStaticallyRequiredOccurrencesInDataRepresentation: Boolean

  /**
   * True if the term has some syntax itself or recursively within itself that
   * must appear in the data stream.
   *
   * False only if the term has possibly no representation whatsoever in the
   * data stream.
   */
  def hasKnownRequiredSyntax: Boolean

  /**
   * Can have a varying number of occurrences.
   *
   * Overridden for elements. See [[ParticleMixin.isVariableOccurrences]]
   */
  def isVariableOccurrences: Boolean = false

  /*
   * This function returns at list of simple elements that are descendents of
   * this term that are not defaultable or OVC. This is a requirement for terms
   * inside a hidden group. Note that there is an exception for choices, in
   * which only a single branch be all defaultable or OVC. If any elements in a
   * hidden group are not defaultable or OVC, then it is an SDE. This function
   * assumes it is only called on elements inside of a hidden group.
   *
   * Note that this currently only requires OVC since default's aren't
   * implemented. This function may need to change when we support defaults.
   */
  lazy val childrenInHiddenGroupNotDefaultableOrOVC: Seq[ElementBase] = Nil
  // {
  //    // this should only be called on hidden elements
  //    val isH = isHidden
  //    Assert.invariant(isH)
  //
  //    val res = this match {
  //      case s: SequenceTermBase => {
  //        s.groupMembers.flatMap { member =>
  //          val res = member.childrenInHiddenGroupNotDefaultableOrOVC
  //          res
  //        }
  //      }
  //      case c: ChoiceTermBase => {
  //        val branches = c.groupMembers.map { _.childrenInHiddenGroupNotDefaultableOrOVC }
  //        val countFullyDefaultableOrOVCBranches = branches.count { _.length == 0 }
  //        if (countFullyDefaultableOrOVCBranches == 0) {
  //          c.SDE("xs:choice inside a hidden group must contain a branch with all children having the dfdl:outputValueCalc property set.")
  //          // TODO: Diagnostics to display which branches contained non-defaultable elements, and what those elements were
  //        }
  //        Nil
  //      }
  //      case e: ElementBase if e.isComplexType => {
  //        e.complexType.group.childrenInHiddenGroupNotDefaultableOrOVC
  //      }
  //      case e: ElementBase => {
  //        if (!e.canBeAbsentFromUnparseInfoset) {
  //          Seq(e)
  //        } else {
  //          Nil
  //        }
  //      }
  //    }
  //    res
  //  }

  final protected lazy val realChildren: Seq[Term] = {
    this match {
      case mg: ModelGroup => mg.groupMembers.asInstanceOf[Seq[Term]]
      case eb: ElementBase if (eb.isComplexType) => Seq(eb.complexType.group)
      case eb: ElementBase => Seq()
    }
  }

}
