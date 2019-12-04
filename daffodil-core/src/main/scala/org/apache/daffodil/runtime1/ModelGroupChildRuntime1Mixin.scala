package org.apache.daffodil.runtime1

import org.apache.daffodil.dsom._

trait ModelGroupChildRuntime1Mixin { self: ModelGroupChild =>

}

trait SequenceChildRuntime1Mixin
  extends ModelGroupChildRuntime1Mixin { self: SequenceChild =>

}

trait ChoiceChildRuntime1Mixin
  extends ModelGroupChildRuntime1Mixin { self: ChoiceChild =>

}
