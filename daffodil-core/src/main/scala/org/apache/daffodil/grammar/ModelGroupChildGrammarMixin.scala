package org.apache.daffodil.grammar

import org.apache.daffodil.dsom._
import org.apache.daffodil.runtime1._
import org.apache.daffodil.grammar.primitives.MandatoryTextAlignment

trait ModelGroupChildGrammarMixin
  extends InitiatedTerminatedGrammarMixin
  with ModelGroupChildRuntime1Mixin { self: ModelGroupChild =>

  def modelGroupChildContentBody: Gram
}

trait SequenceChildGrammarMixin
  extends ModelGroupChildGrammarMixin
  with SequenceChildRuntime1Mixin { self: SequenceChild =>

}

trait ChoiceChildGrammarMixin
  extends ModelGroupChildGrammarMixin
  with ChoiceChildRuntime1Mixin { self: ChoiceChild =>

}
