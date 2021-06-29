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

package org.apache.daffodil.util

/**
 * Delay[T]
 *
 * This Delayed evaluation technique is an alternative to staggered
 * multi-stage factories, or currying. Turns out currying works fine in
 * Scala for functions, but not for class constructors, so it's easier to
 * make the class constructor take the usual bunch of arguments, but the
 * ones we might have wanted to supply later, we instead supply them, but
 * as Delay objects.
 *
 * For more info, this is used in the IncludeImport stuff in DSOM, and
 * in numerous other places where we construct, functionally, cyclic graphs.
 */
final class Delay[T] private (private var box: Delay.Box[T])
  extends PreSerialization {
  //
  // This trick of taking another object on a var, and
  // then clobbering the var after we demand the value
  // eliminates holding onto a bunch of objects due
  // to closures for the pass-by-name arguments.
  //
  // The idea is no matter what the Scala implementation is doing in its implementation of
  // by-name args, that's on the constructor of the box object,
  // and we're going to explicitly null-out the reference to the box object
  // which guarantees we're not holding onto things we don't expect
  // once the Delay object has been evaluated.
  //
  lazy val value: T = {
    val v = box.value
    box = null // throws away box, which allows GC of all closures, etc.
    // Delay.evalDelay()
    v
  }

  /**
   * For creating a delay object purely to satisfy a type requirement
   * when you know the argument does not actually need to be delayed.
   */
  def force : Delay[T] = { value; this }

  /**
   * Create a string representation. Does not force the value to be computed.
   */
  override def toString = {
    val bodyString = if (box eq null) value.toString else "..."
    "Delay(" + bodyString + ")"
  }

  override def preSerialization = {
    value // force evaluation
    super.preSerialization
  }

  @throws(classOf[java.io.IOException])
  final private def writeObject(out: java.io.ObjectOutputStream): Unit = serializeObject(out)
}

object Delay {
  /**
   * Create a delayed expression object.
   *
   * @param delayedExpression an argument expression which will not be evaluated until required
   * @tparam T type of the argument. (Usually inferred by Scala.)
   * @return the Delay object
   */
  def apply[T](delayedExpression: => T) = {
    // allocateDelay()
    new Delay(new Box(delayedExpression))
  }

  /**
   * Specifically, this is NOT serializable.
   * Serialization must force all Delay objects.
   */
  private class Box[T](delayedExpression: => T) {
    def value = delayedExpression
  }
}
