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

package org.apache.daffodil.runtime1

import org.apache.daffodil.dsom.LocalElementDeclBase

trait LocalElementDeclBaseRuntime1Mixin { self: LocalElementDeclBase =>

  // needed to initialize cyclic structures of ERDs of Quasi-Elements
  requiredEvaluationsAlways(
    // Puzzle why is this needed if all components get the setRequiredEvaluationsActive()
    // call (in Root.scala) and ElementBase has this initialization under
    // a requiredEvaluationsIfActive(...)
    //
    // It must be that some components don't get put on the allComponents list of Root.
    // Most likely that is the quasi-elements.
    elementRuntimeData.initialize
  )
}
