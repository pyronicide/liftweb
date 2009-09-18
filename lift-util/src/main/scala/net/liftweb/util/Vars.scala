/*
 * Copyright 2007-2009 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions
 * and limitations under the License.
 */
package net.liftweb.util

import Helpers._
import base._

/**
 * Abstract a request or a session scoped variable.
 */
abstract class AnyVar[T, MyType <: AnyVar[T, MyType]](dflt: => T) extends PSettableValueHolder[T] {
  self: MyType =>
  protected lazy val name = "_lift_sv_"+getClass.getName+"_"+__nameSalt
  protected def findFunc(name: String): Box[T]
  protected def setFunc(name: String, value: T): Unit
  protected def clearFunc(name: String): Unit
  protected def wasInitialized(name: String): Boolean

  protected def __nameSalt = ""

  type CleanUpParam

  /**
   * The current value of the variable
   */
  def is: T = synchronized {
    findFunc(name) match {
      case Full(v) => v
      case _ => val ret = dflt
        testInitialized
        apply(ret)
        ret
    }
  }

  private def testInitialized: Unit = synchronized {
    if (!wasInitialized(name)) {
      registerCleanupFunc(_onShutdown _)
    }
  }

  /**
   * Shadow of the 'is' method
   */
  def get: T = is

  /**
   * Shadow of the apply method
   */
  def set(what: T): T = apply(what)

  /**
   * Set the session variable
   *
   * @param what -- the value to set the session variable to
   */
  def apply(what: T): T = {
    testInitialized
    setFunc(name, what)
    what
  }

  /**
   * Applies the given function to the contents of this
   * variable and sets the variable to the resulting value.
   *
   * @param f -- the function to apply and set the result from.
   */
  def update(f: T => T): T = {
    apply(f(is))
    is
  }

  def remove(): Unit = clearFunc(name)

  //def cleanupFunc: Box[() => Unit] = Empty

protected def registerCleanupFunc(in: CleanUpParam => Unit): Unit

  protected final def registerGlobalCleanupFunc(in: CleanUpParam => Unit) {
    cuf ::= in
  }

  private var cuf: List[CleanUpParam => Unit] = Nil

  private def _onShutdown(session: CleanUpParam): Unit = {
    cuf.foreach(f => tryo(f(session)))
    onShutdown(session)
  }

  protected def onShutdown(session: CleanUpParam): Unit = {}

  override def toString = is.toString

  /**
   * Change the value of the Var for the lifespan of the function
   */
  def doWith[F](newVal: T)(f: => F): F = {
    val old = findFunc(name)
    setFunc(name, newVal)
    try {
      f
    } finally {
      old match {
        case Full(t) => setFunc(name, t)
        case _ => clearFunc(name)
      }
    }
  }
}

abstract class NonCleanAnyVar[T](dflt: => T) extends AnyVar[T, NonCleanAnyVar[T]](dflt) {
  type CleanUpParam = Unit
  override protected def registerCleanupFunc(in: Unit => Unit): Unit = {}
}

object AnyVar {
  implicit def whatVarIs[T](in: AnyVar[T, _]): T = in.is
}
