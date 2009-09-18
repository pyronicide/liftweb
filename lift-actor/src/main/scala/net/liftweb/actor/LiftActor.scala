/*
 * Copyright 2009 WorldWide Conferencing, LLC
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

package net.liftweb.actor
import base._
import util._
import Helpers._

trait ILAExecute {
  def execute(f: () => Unit): Unit
  def shutdown(): Unit
}

object LAScheduler {
  @volatile
  var onSameThread = false

  @volatile
  var createExecutor: () => ILAExecute = () => {
    new ILAExecute {
      import _root_.java.util.concurrent.{Executors, Executor}

      private val es: Executor = Executors.newCachedThreadPool()

      def execute(f: () => Unit): Unit =
      es.execute(new Runnable{def run() {f()}})

      def shutdown(): Unit = {}
    }
  }

  @volatile
  var exec: ILAExecute = _

  def execute(f: () => Unit) {touch; exec.execute(f)}

  private lazy val touch = {
    exec = createExecutor()
    true
  }

  def shutdown() {}
}

trait SpecializedLiftActor[T] extends SimpleActor[T]  {
  @volatile
  private[this] var processing = false
  @volatile  private[this] val baseMailbox: MailboxItem = new SpecialMailbox
  @volatile private[this] var msgList: List[T] = Nil
  @volatile private[this] var startCnt = 0

  private class MailboxItem(val item: T) {
    var next: MailboxItem = _
    var prev: MailboxItem = _

    def find(f: MailboxItem => Boolean): Box[MailboxItem] =
    if (f(this)) Full(this) else next.find(f)

    def remove() {
      val newPrev = prev
      prev.next = next
      next.prev = prev
    }

    def insertAfter(newItem: MailboxItem): MailboxItem = {
      next.prev = newItem
      newItem.prev = this
      newItem.next = this.next
      next = newItem
      newItem
    }

    def insertBefore(newItem: MailboxItem): MailboxItem = {
      prev.next = newItem
      newItem.prev = this.prev
      newItem.next = this
      prev = newItem
      newItem
    }
  }

  private class SpecialMailbox extends MailboxItem(null.asInstanceOf[T]) {
    override def find(f: MailboxItem => Boolean): Box[MailboxItem] = Empty
    next = this
    prev = this
  }

  def !(msg: T): Unit = {
    val toDo: () => Unit = baseMailbox.synchronized {
      msgList ::= msg
      if (!processing) {
        if (LAScheduler.onSameThread) {
          processing = true
          () => processMailbox(true)
        } else {
          if (startCnt == 0) {
            startCnt += 1
            () => LAScheduler.execute(() => processMailbox(false))
          } else
          () => {}
        }
      }
      else () => {}
    }
    toDo()
  }

  private def processMailbox(ignoreProcessing: Boolean) {
    var clearProcessing = true
    baseMailbox.synchronized {
      if (!ignoreProcessing && processing) return
      processing = true
      if (startCnt > 0) startCnt = 0
    }

    val eh = exceptionHandler

    def putListIntoMB(): Unit = {
      msgList.foldLeft(baseMailbox)((mb, msg) => mb.insertBefore(new MailboxItem(msg)))
      msgList = Nil
    }

    try {
      while (true) {
        val pf = messageHandler
        baseMailbox.synchronized {putListIntoMB()}
        baseMailbox.next.find(v => pf.isDefinedAt(v.item)) match {
          case Full(mb) =>
            mb.remove()
            try {
              pf(mb.item)
            } catch {
              case e: Exception => if (eh.isDefinedAt(e)) eh(e)
            }
          case _ => 
            baseMailbox.synchronized {
              if (msgList.isEmpty) {
                processing = false
                clearProcessing = false
                return
              }
              else {
                putListIntoMB()
              }
            }
        }
      }
    } catch {
      case e =>
        if (eh.isDefinedAt(e)) eh(e)
        throw e
    } finally {
      if (clearProcessing) {
        baseMailbox.synchronized {
          processing = false
        }
      }
    }
  }

  protected def testTranslate[R](v: T, f: T => R) = f(v)

  protected def execTranslate[R](v: T, f: T => R) = f(v)

  protected def messageHandler: PartialFunction[T, Unit]

  protected def exceptionHandler: PartialFunction[Throwable, Unit] = {
    case e => Log.error("Error processing Actor "+this, e)
  }
}

trait LiftActor extends SpecializedLiftActor[Any] with Actor {
  @volatile
  private[this] var responseFuture: LAFuture[Any] = null

  private case class MsgWithResp(msg: Any, future: LAFuture[Any])

  def !?(msg: Any): Any = {
    val future = new LAFuture[Any]
    this ! MsgWithResp(msg, future)
    future.get
  }

  def !?(timeout: Long, msg: Any): Option[Any] = {
    val future = new LAFuture[Any]
    this ! MsgWithResp(msg, future)
    future.get(timeout)
  }

  override protected def testTranslate[R](v: Any, f: Any => R) = v match {
    case MsgWithResp(msg, _) => f(msg)
    case v => f(v)
  }

  override protected def execTranslate[R](v: Any, f: Any => R) = v match {
    case MsgWithResp(msg, future) =>
      responseFuture = future
      try {
        f(msg)
      } finally {
        responseFuture = null
      }
    case v => f(v)
  }

  protected def reply(v: Any) {
    if (null ne responseFuture) responseFuture.satisfy(v)
  }
}
