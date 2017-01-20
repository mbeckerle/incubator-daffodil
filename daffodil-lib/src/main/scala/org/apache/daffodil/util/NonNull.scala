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

import org.apache.daffodil.equality._
import scala.language.implicitConversions
/**
 * A type that can be used to insist things are not null.
 *
 * Lets say you want to require an argument to be not null.
 * {{{
 *    def foo(nnStr: NotNull[String]) = {
 * }}}
 * You then write code as if the nnStr was just a string.
 * The exception to this is equality comparisons. There you must
 * call nnStr.value because the "==" operator doesn't require
 * that its arguments are both of the same type.
 *
 * If you use strongly typed equality for value types (the "=#=" or "!=#=" operators)
 * however, then it works as expected
 *
 * The point of this class is if you use NonNull[T] instead of T, and live in
 * that world, where you pass NonNull[T] etc. then there is ZERO overhead for this.
 * The type system insures that the objects can never be null, and it is only
 * when you cross from regular nullable objects to constructing NonNulls that
 * there is checking overhead.
 *
 * This notion is copied from the Nice programming language. In Nice the basic
 * type names like String are assumed to behave like NonNull[String], and they
 * use a question mark type constructor "?String" to indicate a nullable String.
 * We can't achieve that, but we can achieve almost equivalent convenience if one
 * uses type synonym definitions such as:
 * {{{
 * import edu.illinois.ncsa.daffodil.equality_
 * import edu.illinois.ncsa.daffodil.NonNull
 * import edu.illinois.ncsa.daffodil.NonNull.Implicits._
 *
 * type NNString = NonNull[String]
 *
 * class MyClass {
 *
 *   def foo(nnStr: NNString): NNString = {
 *      if (nnStr =#= "") nnStr           // Typed equality treats as a  string.
 *      else {
 *          println(nnStr.codePointAt(0)) // Converts implicitly to a string
 *          val nnStr2: NNString =
 *              nnStr.substring(1)        // Converts implicitly from a string
 *          foo(nnStr2)                   // No checking: arg is known NNString
 *       }
 *   }
 *
 *   def bar: Unit = {
 *      val s = returnsAString()
 *      foo(s)                           // Check occurs here as we are
 *                                       // crossing from nullable to NonNull type.
 *   }
 * }
 * }}}
 * All the above occurs with zero overhead. Literally generates no code
 * at all except when converting from nullable to NonNull type.
 *
 * The scala type system is just keeping
 * places where we have to check for null and places where we don't separate, and
 * insuring when we cross from nullable to NonNull types, that a check is done,
 * and it need not be done elsewhere.
 */
final class NonNull[+T <: AnyRef] private (val v: T) extends AnyVal with Serializable {
  @inline final def value: T = v // .asInstanceOf[T]
  @inline final def toMaybe: Maybe[T] = new Maybe[T](v)
  @inline final def toOption: scala.Option[T] = scala.Some(value)
  override final def toString = value.toString
}

object NonNull {

  import scala.language.implicitConversions

  def npe = throw new NullPointerException("Cannot construct a NotNull from null.")

  @inline
  def apply[T <: AnyRef](value: T) =
    if (value eq null) npe
    else new NonNull[T](value)

  object Implicits {

    @inline
    implicit def NotNull_T_To_T[T <: AnyRef](nn: NonNull[T]): T = nn.value

    @inline
    implicit def T_To_NotNull_T[T <: AnyRef](n: T): NonNull[T] = NonNull(n)
  }

  /**
   * Example of how to use it.
   */
  private[util] object ExampleCode {
    import Implicits._

    /**
     *  Use type synonyms for brevity.
     */
    type NNString = NonNull[String]

    def foo(nnStr: NNString): NNString = {
      if (nnStr =#= "") nnStr // typed equality treats as a  string.
      else {
        println(nnStr.codePointAt(0)) // converts implicitly to a string
        val nnStr2: NNString = nnStr.substring(1) // converts implicitly from a string
        foo(nnStr2) // no checking if arg is already known to be a NNString
      }
    }

    def bar: Unit = {
      val s = returnsAString()
      foo(s) // check occurs here as we are crossing from nullable to NonNull type.
    }

    private def returnsAString(): String = "some string"
  }

  // TODO: Verify by small perf-tests, and by reading the byteCode, that this
  // in fact has no overhead.

}

