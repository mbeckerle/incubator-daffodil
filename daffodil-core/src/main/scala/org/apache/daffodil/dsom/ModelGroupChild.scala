package org.apache.daffodil.dsom

import org.apache.daffodil.grammar._
import org.apache.daffodil.schema.annotation.props._
import org.apache.daffodil.schema.annotation.props.gen._
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.api.WarnID
import org.apache.daffodil.infoset.ChoiceBranchEvent

trait ModelGroupChild
  extends SchemaComponent
  with ModelGroupChildGrammarMixin
  with ImplementsThrowsOrSavesSDE {

  requiredEvaluations(checkForAlignmentAmbiguity)

  def parentGroupDef: GroupDefLike

  def childTerm: Term

  def position: Int

  final def hasDelimiters = childTerm.hasDelimiters
  final def hasEncoding = childTerm.hasEncoding
  final def hasKnownRequiredSyntax = childTerm.hasKnownRequiredSyntax
  final def hasStaticallyRequiredOccurrencesInDataRepresentation =
    childTerm.hasStaticallyRequiredOccurrencesInDataRepresentation
  final def hasTerminator = childTerm.hasTerminator
  final def isArray = childTerm.isArray
  final def isOptional = childTerm.isOptional
  final def isRepresented = childTerm.isRepresented
  final def isScalar = childTerm.isScalar
  final def knownEncodingAlignmentInBits = childTerm.knownEncodingAlignmentInBits
  final def statements = childTerm.statements

  /**
   * nearestEnclosingSequence
   *
   * An attribute that looks upward to the surrounding
   * context of the schema, and not just lexically surrounding context. It needs to see
   * what declarations will physically surround the place. This is the dynamic scope,
   * not just the lexical scope. So, a named global type still has to be able to
   * ask what sequence is surrounding the element that references the global type.
   *
   * This is why we have to have the GlobalXYZDefFactory stuff. Because this kind of back
   * pointer (contextual sensitivity) prevents sharing.
   */
  //  final lazy val nearestEnclosingSequence: Option[SequenceTermBase] = enclosingTerm match {
  //    case None => None
  //    //
  //    // We want to recurse outward, and don't care about these implied sequence terms
  //    // that choices wrap around branch elements.
  //    //
  //    case Some(cs: ChoiceBranchImpliedSequence) => enclosingTerm.get.nearestEnclosingSequence
  //    case Some(s: SequenceTermBase) => Some(s)
  //    case Some(_) => enclosingTerm.get.nearestEnclosingSequence
  //  }
  //
  //  final lazy val nearestEnclosingChoiceBeforeSequence: Option[ChoiceTermBase] = enclosingTerm match {
  //    case None => None
  //    //
  //    // We want to recurse outward, and don't care about these implied sequence terms
  //    // that choices wrap around branch elements.
  //    //
  //    case Some(cs: ChoiceBranchImpliedSequence) => enclosingTerm.get.nearestEnclosingChoiceBeforeSequence
  //    case Some(s: SequenceTermBase) => None
  //    case Some(c: ChoiceTermBase) => Some(c)
  //    case Some(_) => enclosingTerm.get.nearestEnclosingChoiceBeforeSequence
  //  }
  //
  //  final lazy val nearestEnclosingUnorderedSequence: Option[SequenceTermBase] = enclosingTerm match {
  //    case None => None
  //    case Some(s: SequenceTermBase) if !s.isOrdered => Some(s)
  //    case Some(_) => enclosingTerm.get.nearestEnclosingUnorderedSequence
  //  }
  //
  //  final lazy val isInUnorderedSequence: Boolean = !nearestEnclosingUnorderedSequence.isEmpty
  //
  //  final lazy val nearestEnclosingUnorderedSequenceBeforeSequence: Option[SequenceTermBase] = enclosingTerm match {
  //    case None => None
  //    case Some(s: SequenceTermBase) if !s.isOrdered => Some(s)
  //    case Some(s: SequenceTermBase) => None
  //    case Some(_) => enclosingTerm.get.nearestEnclosingUnorderedSequence
  //  }
  //
  //  final lazy val inChoiceBeforeNearestEnclosingSequence: Boolean = enclosingTerm match {
  //    case None => false
  //    case Some(s: SequenceTermBase) => false
  //    case Some(c: ChoiceTermBase) => true
  //    case Some(_) => enclosingTerm.get.inChoiceBeforeNearestEnclosingSequence
  //  }
  //
  //  final lazy val nearestEnclosingElement: Option[ElementBase] = enclosingTerm match {
  //    case None => None
  //    case Some(eb: ElementBase) => Some(eb)
  //    case Some(_) => enclosingTerm.get.nearestEnclosingElement
  //  }

  @deprecated("2019-12-04", "Use parentGroupDef. No option needed.")
  final lazy val immediatelyEnclosingGroupDef: Option[GroupDefLike] = Option(parentGroupDef)
  //  {
  //    optLexicalParent.flatMap { lexicalParent =>
  //      val res: Option[GroupDefLike] = lexicalParent match {
  //        case c: Choice => Some(c)
  //        //
  //        // skip past the implied sequence that is wrapped around choice branches
  //        // to the actual choice
  //        //
  //        case c: ChoiceBranchImpliedSequence => c.immediatelyEnclosingGroupDef
  //        case s: Sequence => Some(s)
  //        case d: SchemaDocument => {
  //          // we must be the Root elementRef or a quasi node
  //          Assert.invariant(this.isInstanceOf[Root] || this.isInstanceOf[QuasiElementDeclBase])
  //          None
  //        }
  //        case gdd: GlobalGroupDef => Some(gdd)
  //        case ctd: ComplexTypeBase => None
  //
  //        case _ => Assert.invariantFailed("immediatelyEnclosingModelGroup called on " + this + " with lexical parent " + lexicalParent)
  //      }
  //      res
  //    }
  //  }

  lazy val immediatelyEnclosingModelGroup: Option[ModelGroup] =
    immediatelyEnclosingGroupDef.flatMap {
      _ match {
        case mg: ModelGroup => Some(mg)
        case _ => None
      }
    }

  /**
   * One-based position in the nearest enclosing sequence.
   * Follows backpointers from group defs to group refs until it
   * finds a sequence.
   */
  //  final lazy val positionInNearestEnclosingSequence: Int = {
  //    // FIXME:Classic example of a method that creates a pointless
  //    // need for the backpointers to parent that make structure
  //    // sharing of the DSOM objects impossible. This should be a value
  //    // passed down to a SequenceChild constructor, and the algorithms
  //    // that use this should be on SequenceTermBase or child classes thereof.
  //    val optET = enclosingTerm
  //    val optNES = nearestEnclosingSequence
  //    val res =
  //      (optET, optNES) match {
  //        case (Some(et), Some(nes)) if (et == nes) =>
  //          position
  //        case _ => {
  //          if (this.isInstanceOf[Root]) 1
  //          else {
  //            optET match {
  //              case Some(term: Term) => term.positionInNearestEnclosingSequence
  //              case x => Assert.invariantFailed("For " + this + " unable to compute position in nearest enclosing sequence. The enclosingComponent was " + x)
  //            }
  //          }
  //        }
  //      }
  //    res
  //  }
  //
  //  final lazy val allSiblings: Seq[Term] = {
  //    val res = nearestEnclosingSequence.map { enc =>
  //      val allSiblings = enc.groupMembers
  //      allSiblings
  //    }
  //    res.getOrElse(Nil)
  //  }
  //
  final lazy val priorSiblings = {
    optLexicalParent match {
      case Some(stb: SequenceTermBase) => stb.groupMembers.take(position - 1)
      case Some(gsgd: GlobalSequenceGroupDef) => gsgd.groupMembers.take(position - 1)
      case _ => Nil
    }
  }

  /**
   * Siblings in the lexically enclosing group.
   */
  final lazy val laterSiblings = {
    optLexicalParent match {
      case Some(stb: SequenceTermBase) => stb.groupMembers.drop(position)
      case Some(gsgd: GlobalSequenceGroupDef) => gsgd.groupMembers.drop(position)
      case _ => Nil
    }
  }
  //  final lazy val laterElementSiblings = laterSiblings.collect { case elt: ElementBase => elt }
  //
  final lazy val priorSibling = priorSiblings.lastOption
  final lazy val nextSibling = laterSiblings.headOption
  //
  //  final lazy val priorPhysicalSiblings = priorSiblings.filter { _.isRepresented }
  //  final lazy val priorPhysicalSibling = priorPhysicalSiblings.lastOption

  //
  // FIXME: incomplete analysis. This needs to walk outward to parent, then down into
  // last of prior sibling sequence group looking downward at last child until it finds
  // a physical term that satisfies the test.
  // E.g., the prior sibling in this sequence might satisfy, or the enclosing parent if we're
  // first, or the prior sibling of the enclosing parent, or the last child of the prior
  // sibling of the enclosing parent, and so on.
  //
  // Really we often need the "things before this" enumerated and filtered, so there
  // should be a stream of things looking at prior prior of that, prior of that, etc.
  //
  // Choice groups require special consideration. A prior that's a choice only has a
  // defined property if (1) it's relevant to the choice group - so dfdl:terminator yes, dfdl:byteOrder no.
  // (2) it is present for that choice group, or (3) it is recursively present on the last of
  // ALL children of the choice group, so that it is present with a specific value no matter
  // which branch of the choice is realized.
  //
  // It is ok for this to stop early and be less comprehensive about walking backward
  // IFF it is used in conservative analysis, i.e., where not finding the term, even if
  // it does in fact exist back there someplace, causes no incorrectness, just suboptimality.
  //
  // Note also that the predicate test interacts with sequence groups in a complex way.
  // If the sequence group has separators, the separator will be present (because the current
  // term is not first, or the sep is in prefix position, or ...) then if the predicate
  // is true of a sequence separator (e.g., such as has same encoding property value) then
  // we have a preceding physical term, the enclosing sequence, which has a physical
  // syntax, the separator, which satisfies the predicate.
  //
  // That's the job of the predicate. The point is that this predicate may or may not
  // stop on some enclosing parent, depending on separators, etc. You can't just have the
  // predicate be "has same encoding" test, because whether that encoding will have been
  // put into effect depends on whether some group syntax - such as a separator, or initiator
  // will have been present and so required establishing that property to be in effect.
  //
  // If a sequence has no separator and no initiator, then it doesn't impose an encoding prior to or
  // between the sibling children. Hence, even if it has an encoding property in scope, and even
  // uses it for a terminator, it doesn't re-establish that encoding prior to children, so
  // the analysis can't stop on the sequence.
  //  final def nearestPriorPhysicalTermSatisfying(pred: Term => Boolean): Option[Term] = {
  //    priorPhysicalSiblings.filter { pred(_) }.lastOption match {
  //      case x @ Some(sib) => x
  //      case None => {
  //        // must try enclosing terms outward
  //        enclosingTerm match {
  //          case None => None
  //          case x @ Some(t) if pred(t) => x
  //          case Some(t) => t.nearestPriorPhysicalTermSatisfying(pred)
  //        }
  //      }
  //    }
  //  }
  //
  final lazy val hasLaterRequiredSiblings = laterSiblings.exists(_.childTerm.hasStaticallyRequiredOccurrencesInDataRepresentation)
  final lazy val hasPriorRequiredSiblings = priorSiblings.exists(_.childTerm.hasStaticallyRequiredOccurrencesInDataRepresentation)

  /**
   * The concept of potentially trailing is defined in the DFDL specification.
   *
   * This concept applies to terms that are direct children of a sequence only.
   *
   * It is true for terms that may be absent from the representation, but furthermore, may be last
   * in a sequence, so that the notion of whether they are trailing, and so their separator may not be
   * present, is a relevant issue.
   *
   * If an element is an array, and has some required instances, then it is not potentially trailing, as some
   * instances will have to appear, with separators.
   *
   * This concept applies only to elements and model groups that have representation in the data stream.
   *
   * Previously there was a misguided notion that since only DFDL elements can have minOccurs/maxOccurs
   * that this notion of potentially trailing didn't apply to model groups. (Sequences and Choices, the other
   * kind of Term). But this is not the case.
   *
   * A sequence/choice which has no framing, and whose content doesn't exist - no child elements, any contained
   * model groups recursively with no framing and no content - such a model group effectively "dissapears" from
   * the data stream, and in some cases need not have a separator.
   *
   * This is computed by way of couldBePotentiallyTrailing. This value means that the term, in isolation, looking only
   * at its own characteristics, disregarding its following siblings in any given sequence, has the characteristics
   * of being potentially trailing.
   *
   * Then that is combined with information about following siblings in a sequence to determine if a given term, that
   * is a child of a sequence, is in fact potentially trailing within that sequence.
   *
   * These two concepts are mutually recursive, since a sequence that is entirely composed of potentially trailing children
   * satisfies couldBePotentialyTrailing in whatever sequence encloses it.
   */
  final lazy val isPotentiallyTrailing: Boolean = {
    val thisCouldBe = couldBePotentiallyTrailing
    lazy val laterSibilingsAre = laterSiblings.forall { _.isPotentiallyTrailing }
    val res = thisCouldBe && laterSibilingsAre
    res
  }

  final lazy val couldBePotentiallyTrailing: Boolean = {
    import SeparatorSuppressionPolicy._
    this match {
      case e: ElementBase => {
        lazy val allowsZeroOccurs = e.minOccurs == 0
        lazy val minOccursNotZeroButDeclaredLast = !e.isScalar && e.minOccurs > 0 && e.isLastDeclaredRepresentedInSequence
        lazy val hasAllowedOCK = (e.occursCountKind eq OccursCountKind.Implicit) ||
          (e.occursCountKind eq OccursCountKind.Parsed)
        lazy val hasAllowedLengthKind = e.lengthKind eq LengthKind.Delimited
        lazy val hasNoDiscriminators = !statements.exists { s => s.isInstanceOf[DFDLDiscriminator] }
        val res =
          isRepresented &&
            (allowsZeroOccurs ||
              minOccursNotZeroButDeclaredLast) &&
              hasAllowedOCK &&
              hasAllowedLengthKind &&
              hasNoDiscriminators
        res
      }
      case m: ModelGroup => {
        lazy val seqIsNotSSPNever = m match {
          case s: SequenceTermBase =>
            s.hasSeparator &&
              (s.separatorSuppressionPolicy match {
                case TrailingEmpty => true
                case TrailingEmptyStrict => true
                case AnyEmpty => true
                case Never => false
              })
          case _ => true
        }
        lazy val hasNoStatements = statements.length == 0
        lazy val recursivelyOk =
          m.representedMembers.forall { m =>
            m.couldBePotentiallyTrailing
          }
        val res =
          isRepresented &&
            !m.hasFraming &&
            seqIsNotSSPNever &&
            hasNoStatements &&
            recursivelyOk
        res
      }
    }
  }
  /**
   * Returns a tuple, where the first item in the tuple is the list of sibling
   * terms that could appear before this. The second item in the tuple is a
   * One(enclosingParent) if all prior siblings are optional or this element has no prior siblings
   */
  //  lazy val potentialPriorTerms: (Seq[Term], Option[Term]) = LV('potentialPriorTerms) {
  //    val et = enclosingTerm
  //    val (potentialPrior, optEnclosingParent) = et match {
  //      case None => (Seq(), None)
  //      case Some(eb: ElementBase) => (Seq(), Some(eb))
  //      case Some(ch: ChoiceTermBase) => (Seq(), Some(ch))
  //      case Some(sq: SequenceTermBase) if !sq.isOrdered => {
  //        (sq.groupMembers, Some(sq))
  //      }
  //      case Some(sq: SequenceTermBase) if sq.isOrdered => {
  //        val previousTerms = sq.groupMembers.takeWhile { _ != this }
  //        if (previousTerms.isEmpty) {
  //          // first child of seq, the seq is the only previous term
  //          (Seq(), Some(sq))
  //        } else {
  //          val firstNonOptional = previousTerms.reverse.find {
  //            _ match {
  //              case eb: ElementBase if !eb.isRequiredStreamingUnparserEvent || !eb.isRepresented => false
  //              case _ => true
  //            }
  //          }
  //          if (firstNonOptional.isEmpty) {
  //            // all previous siblings are optional, all or the seq could be previous
  //            (previousTerms, Some(sq))
  //          } else {
  //            // drop all siblings up until the first non optional
  //            (previousTerms.dropWhile { _ != firstNonOptional.get }, None)
  //          }
  //        }
  //      }
  //    }
  //    val potentialPriorRepresented = potentialPrior.filter { term =>
  //      term match {
  //        case eb: ElementBase => eb.isRepresented
  //        case _ => true
  //      }
  //    }
  //    (potentialPriorRepresented, optEnclosingParent)
  //  }.value

  //
  //  final lazy val possibleNextTerms: Seq[Term] = LV('possibleNextTerms) {
  //    val es = this.nearestEnclosingSequence
  //    val eus = this.nearestEnclosingUnorderedSequenceBeforeSequence
  //    val ec = this.nearestEnclosingChoiceBeforeSequence
  //
  //    val enclosingUnorderedGroup = {
  //      (ec, eus) match {
  //        case (None, None) => None
  //        case (Some(choice), _) => Some(choice)
  //        case (None, Some(uoSeq)) => Some(uoSeq)
  //      }
  //    }
  //    val listOfNextTerm = (enclosingUnorderedGroup, es) match {
  //      case (None, None) => Seq.empty
  //      case (Some(unorderedGroup), _) => {
  //        // We're in a choice or unordered sequence
  //        //
  //        // List must be all of our peers since (as well as our self)
  //        // we could be followed by any of them plus
  //        // whatever follows the unordered group.
  //        val peersCouldBeNext = unorderedGroup.groupMembers
  //
  //        val termsUntilFirstRequiredTerm = peersCouldBeNext ++ unorderedGroup.possibleNextTerms
  //        termsUntilFirstRequiredTerm
  //      }
  //      case (None, Some(oSeq)) => {
  //        // We're in an ordered sequence
  //
  //        val termsUntilFirstRequiredTerm =
  //          isLastDeclaredRepresentedInSequence match {
  //            case true => oSeq.possibleNextTerms
  //            case false => {
  //
  //              val members = oSeq.groupMembers
  //
  //              val selfAndAfter = members.dropWhile(m => m ne this)
  //              val after = selfAndAfter.drop(1)
  //              val nextMember = after.headOption
  //
  //              val nextMembers =
  //                nextMember match {
  //                  case Some(e: ElementBase) if e.isOptional => Seq(e) ++ e.possibleNextTerms
  //                  case Some(e: ElementBase) => Seq(e)
  //                  case Some(mg: ModelGroup) => Seq(mg)
  //                  case None => Nil // Assert.impossibleCase
  //                }
  //              nextMembers
  //            }
  //          }
  //        termsUntilFirstRequiredTerm
  //      }
  //    }
  //    listOfNextTerm
  //  }.value

  /**
   * True if this term is the last one in the enclosing sequence that is represented
   * in the data stream. That is, it is not an element with dfdl:inputValueCalc.
   *
   * This means whether the enclosing sequence's separator (if one is defined) is
   * relevant.
   */
  final lazy val isLastDeclaredRepresentedInSequence = {
    val res = laterSiblings.forall(!_.isRepresented)
    res
  }

  // protected def possibleFirstChildTerms: Seq[Term]

  //  /*
  //   * Returns list of Elements that could be the first child in the infoset of this model group or element.
  //   */
  //  final def possibleFirstChildElementsInInfoset: Seq[ElementBase] = LV('possibleFirstChildElementsInInfoset) {
  //    val pfct = possibleFirstChildTerms
  //    val firstChildren = pfct.flatMap {
  //      case e: ElementBase if e.isHidden => Nil
  //      case e: ElementBase => Seq(e)
  //      case s: SequenceTermBase if s.isHidden => Nil
  //      case mg: ModelGroup => mg.possibleFirstChildElementsInInfoset
  //    }
  //    firstChildren.distinct
  //  }.value

  /*
   * Returns a list of Elements that could follow this Term, including
   * siblings, children of siblings, and siblings of the enclosingParent and their children.
   *
   * What stops this is when the end of an enclosing element has to be next.
   */
  //  final def possibleNextChildElementsInInfoset: Seq[ElementBase] = LV('possibleNextChildElementsInInfoset) {
  //    val arrayNext = if (isArray) Seq(this.asInstanceOf[ElementBase]) else Nil
  //
  //    val nextSiblingElements = {
  //      val poss = possibleNextSiblingTerms
  //      val res = poss.flatMap {
  //        possible =>
  //          possible match {
  //            case e: ElementBase => Seq(e)
  //            case mg: ModelGroup => mg.possibleFirstChildElementsInInfoset
  //          }
  //      }
  //      res
  //    }
  //
  //    val nextParentElts = nextParentElements
  //    val res = arrayNext ++ nextSiblingElements ++ nextParentElts
  //    res
  //  }.value

  // def nextParentElements: Seq[ElementBase]

  // protected def couldBeLastElementInModelGroup: Boolean

  //  /*
  //   * Returns a list of sibling Terms that could follow this term. This will not
  //   * return any children of sibling Terms, or any siblings of the enclosingParent.
  //   */
  //  final def possibleNextSiblingTerms: Seq[Term] = LV('possibleNextSiblingTerms) {
  //    enclosingTerms.flatMap { et =>
  //      val listOfNextTerm = et match {
  //        case e: ElementBase => Nil // complex element, cannot have another model group other than this one
  //        case c: ChoiceTermBase => Nil // in choice, no other siblings could come after this one
  //        case s: SequenceTermBase if !s.isOrdered => s.groupMembers // unorderd sequence, all siblings (and myself) could be next
  //        case s: SequenceTermBase => {
  //          // in a sequence, the next term could be any later sibling that is not
  //          // or does not have a required element, up to and including the first
  //          // term that is/has a required element
  //          //        def isOutputValueCalc(term: Term) =
  //          //          term match { case eb: ElementBase if eb.isOutputValueCalc => true; case _ => false }
  //          val selfAndAllNextSiblings = s.groupMembers.dropWhile(_ != this)
  //          val allNextSiblings = if (selfAndAllNextSiblings.length > 0) selfAndAllNextSiblings.tail else Nil
  //          val nextSiblings = allNextSiblings // .dropWhile(isOutputValueCalc(_))
  //          val (optional, firstRequiredAndLater) = nextSiblings.span {
  //            case e: ElementBase => e.canBeAbsentFromUnparseInfoset
  //            case mg: ModelGroup => !mg.mustHaveRequiredElement
  //          }
  //          optional ++ firstRequiredAndLater.take(1)
  //        }
  //      }
  //      listOfNextTerm
  //    }
  //  }.value

  /**
   * Changed to a warning - DFDL WG decided to make this check optional, but it
   * is still useful as a warning.
   *
   * Turns out that MIL STD 2045 header format needs to pad out to a byte boundary
   * at the end of the structure. An optional, non-byte aligned field precedes
   * the end of the structure; hence, putting a zero-length byte-aligned field
   * at the end was crashing into this error. I couldn't think of a work-around,
   * so changed this into a warning.
   *
   * The old requirement was:
   *   To avoid ambiguity when parsing, optional elements and variable-occurrence arrays
   *   where the minimum number of occurrences is zero cannot have alignment properties
   *   different from the items that follow them. It is a schema definition error otherwise.
   *
   * Part of the required evaluations for ElementBase.
   */
  private lazy val checkForAlignmentAmbiguity: Unit = {
    if (childTerm.isOptional) {
      this.laterSiblings.filterNot(m => m == this).foreach { that =>
        val isSame = childTerm.alignmentValueInBits == that.childTerm.alignmentValueInBits
        if (!isSame) {
          this.SDW(WarnID.AlignmentNotSame, "%s is an optional element or a variable-occurrence array and its alignment (%s) is not the same as %s's alignment (%s).",
            this.toString, childTerm.alignmentValueInBits, that.toString, that.childTerm.alignmentValueInBits)
        }
      }
    }
  }
}

final class SequenceChild(
  override val position: Int,
  override val childTerm: Term,
  val parentSequence: SequenceDefMixin)
  extends SchemaComponentImpl(childTerm.xml, Some(parentSequence))
  with ModelGroupChild
  with SequenceChildGrammarMixin {

  override def parentGroupDef = parentSequence

}

final class ChoiceChild(
  override val position: Int,
  override val childTerm: Term,
  val parentChoice: ChoiceDefMixin)
  extends SchemaComponentImpl(childTerm.xml, Some(parentChoice))
  with ModelGroupChild
  with ChoiceChildGrammarMixin {

  override def parentGroupDef = parentChoice

  def identifyingEventsForChoiceBranch: Seq[ChoiceBranchEvent] = LV('identifyingEventsForChoiceBranch) {
    //    Assert.usage(
    //      immediatelyEnclosingGroupDef.isDefined &&
    //        immediatelyEnclosingGroupDef.get.isInstanceOf[ChoiceTermBase],
    //      "identifyingElementsForChoiceBranch must only be called on children of choices")

    ??? // Copy pattern from calculations for element resolver.

    //    val startEvents = (childrenIdentifiers ++ parentNextIdentifiers).map { e =>
    //      ChoiceBranchStartEvent(e.namedQName)

    // Look at the enclosing terms, and find either the first model group that
    // has required next sibling elements, or find an element. If we find an
    // element without finding such a model group, then the end event of that
    // element could potentially be an identifying event for this model group
    // Otherwise, only start events (either children start events next start
    // events of enclosing model groups) could identify this branch, and no end
    // event could identify this branch. Also note that if this model group
    // must have a required element (i.e. it must contribute to the infost)
    // then none of this matters, and it will not have an identifying end
    // event, since one of the child elements must appear in the infoset.
    //    val endEvent =
    //      if (mustHaveRequiredElement) {
    //        Nil
    //      } else {
    //        var ec = enclosingTerm.get
    //        while (!ec.isInstanceOf[ElementBase] &&
    //          !ec.asInstanceOf[ModelGroup].hasRequiredNextSiblingElement) {
    //          ec = ec.enclosingTerm.get
    //        }
    //        val ee = ec match {
    //          case e: ElementBase => Seq(ChoiceBranchEndEvent(e.namedQName))
    //          case mg: ModelGroup => Nil
    //        }
    //        ee
    //      }
    //
    //    val idEvents = startEvents ++ endEvent
    //    idEvents
  }.value

}
