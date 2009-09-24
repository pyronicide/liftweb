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
package builtin
package snippet

import _root_.scala.xml._
import _root_.net.liftweb.http._
import _root_.net.liftweb.util._
import Helpers._
import Box._

object Comet extends DispatchSnippet {

  def dispatch : DispatchIt = {
    case _ => render _
  }

  def render(kids: NodeSeq) : NodeSeq = {

    Props.inGAE match {
      case true => Text("Comet Disabled in Google App Engine")
      case _ =>  buildComet(kids)
    }
  }

  private def buildSpan(timeb: Box[Long], xml: NodeSeq, cometActor: CometActor, spanId: String): NodeSeq =
  Elem(cometActor.parentTag.prefix, cometActor.parentTag.label, cometActor.parentTag.attributes,
       cometActor.parentTag.scope, Group(xml)) %
  (new UnprefixedAttribute("id", Text(spanId), Null)) %
  (timeb.map(time => (new PrefixedAttribute("lift", "when", Text(time.toString), Null))) openOr Null)
    
  private def buildComet(kids: NodeSeq) : NodeSeq = {

    (for {ctx <- S.session} yield {
       val theType: Box[String] = S.attr.~("type").map(_.text)
       val name: Box[String] = S.attr.~("name").map(_.text)
       try {
         ctx.findComet(theType, name, kids, S.attrsFlattenToMap).map(c =>

            (c !? (26600, AskRender)) match {
              case Some(AnswerRender(response, _, when, _)) if c.hasOuter =>
                buildSpan(Empty, c.buildSpan(when, response.inSpan) ++ response.outSpan, c, c.uniqueId+"_outer")

              case Some(AnswerRender(response, _, when, _)) =>
                c.buildSpan(when, response.inSpan)

              case _ => 
                 buildSpan(Full(0), Comment("FIXME comet type "+theType+" name "+name+" timeout") ++ kids, c, c.uniqueId)
            }) openOr Comment("FIXME - comet type: "+theType+" name: "+name+" Not Found ") ++ kids
          } catch {
            case e => Log.error("Failed to find a comet actor", e); kids
          }
    }) openOr Comment("FIXME: session or request are invalid")
  }
}
