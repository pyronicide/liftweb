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
package net.liftweb
package http

import _root_.net.liftweb.util._
import Helpers._
import _root_.scala.collection.mutable.{HashMap, ListBuffer}

/**
 * Keep session information around without the nastiness of naming session variables
 * or the type-unsafety of casting the results.
 * SessionVars are type-safe variables that map pretty directly to
 * HttpSession attributes.  Put stuff in and they are available for the
 * life of the Session.
 *
 * SessionVar's can be used even from CometActor's as now S scope in a Cometctor is
 * provided automatically.
 *
 * @param dflt - the default value of the session variable
 */
abstract class SessionVar[T](dflt: => T) extends AnyVar[T, SessionVar[T]](dflt) {
  override protected def findFunc(name: String): Box[T] = S.session.flatMap(_.get(name))
  override protected def setFunc(name: String, value: T): Unit = S.session.foreach(_.set(name, value))
  override protected def clearFunc(name: String): Unit = S.session.foreach(_.unset(name))

  override protected def wasInitialized(name: String): Boolean = {
    val bn = name+"_inited_?"
    val old: Boolean = S.session.flatMap(_.get(bn)) openOr false
    S.session.foreach(_.set(bn, true))
    old
  }

  protected override def registerCleanupFunc(in: LiftSession => Unit): Unit =
  S.session.foreach(_.addSessionCleanup(in))

  type CleanUpParam = LiftSession
}

/**
 * Keep request-local information around without the nastiness of naming session variables
 * or the type-unsafety of casting the results.
 * RequestVars share their value through the scope of the current HTTP
 * request.  They have no value at the beginning of request servicing
 * and their value is discarded at the end of request processing.  They
 * are helpful to share values across many snippets.
 *
 * @param dflt - the default value of the session variable
 */
abstract class RequestVar[T](dflt: => T) extends AnyVar[T, RequestVar[T]](dflt) {
  type CleanUpParam = Box[LiftSession]
  override protected def findFunc(name: String): Box[T] = RequestVarHandler.get(name)
  override protected def setFunc(name: String, value: T): Unit = RequestVarHandler.set(name, this, value)
  override protected def clearFunc(name: String): Unit = RequestVarHandler.clear(name)
  override protected def wasInitialized(name: String): Boolean = {
    val bn = name+"_inited_?"
    val old: Boolean = RequestVarHandler.get(bn) openOr false
    RequestVarHandler.set(bn, this, true)
    old
  }

  override protected def registerCleanupFunc(in: Box[LiftSession] => Unit): Unit =
  RequestVarHandler.addCleanupFunc(in)
}

trait CleanRequestVarOnSessionTransition {
  self: RequestVar[T forSome {type T}] =>
}

private[http] object RequestVarHandler {
  private val vals: ThreadGlobal[HashMap[String, (RequestVar[_], Any)]] = new ThreadGlobal
  private val cleanup: ThreadGlobal[ListBuffer[Box[LiftSession] => Unit]] = new ThreadGlobal
  private val isIn: ThreadGlobal[String] = new ThreadGlobal
  private val sessionThing: ThreadGlobal[Box[LiftSession]] = new ThreadGlobal

  private[http] def get[T](name: String): Box[T] =
  for (ht <- Box.legacyNullTest(vals.value);
       v <- ht.get(name).map(_._2).asInstanceOf[Option[T]]) yield v;


  private[http] def set[T](name: String, from: RequestVar[_], value: T): Unit =
  for (ht <- Box.legacyNullTest(vals.value))
  ht(name) = (from, value)

  private[http] def clear(name: String): Unit =
  for (ht <- Box.legacyNullTest(vals.value))
  ht -= name

  private[http] def addCleanupFunc(f: Box[LiftSession] => Unit): Unit =
  for (cu <- Box.legacyNullTest(cleanup.value))
  cu += f

  def apply[T](session: Box[LiftSession], f: => T): T = {
    if ("in" == isIn.value) {
      val tv = vals.value

      // remove all the session variables that are CleanRequestVarOnSessionTransition
      val toRemove: Iterable[String] = tv.flatMap {
        case (name, (it: CleanRequestVarOnSessionTransition, _)) => List(name)
        case _ => Nil
      }

      toRemove.foreach(n => tv -= n)


      sessionThing.set(session)
      f
    } else
    isIn.doWith("in") (
      vals.doWith(new HashMap) (
        cleanup.doWith(new ListBuffer) {
          sessionThing.doWith(session) {
            val ret: T = f

            cleanup.value.toList.foreach(clean => Helpers.tryo(clean(sessionThing.value)))

            ret
          }
        }
      ))
  }
}


object AnyVar {
  implicit def whatSessionVarIs[T](in: SessionVar[T]): T = in.is
  implicit def whatRequestVarIs[T](in: RequestVar[T]): T = in.is
}
