package net.liftweb
package builtin
package snippet

import _root_.scala.xml._
import _root_.net.liftweb.http._
import _root_.net.liftweb.util._
import Box._

object Tail extends DispatchSnippet {

   def dispatch: DispatchIt = {
     case _ => render _
   }

   def render(xhtml: NodeSeq) : NodeSeq = <tail>xhtml</tail>

}
