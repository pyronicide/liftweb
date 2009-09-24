package net.liftweb.util

/*
 * Copyright 2006-2008 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */


import _root_.scala.xml._
import _root_.scala.xml.transform._
import _root_.scala.collection.mutable.ListBuffer

import Helpers._

/**
 * This object provides functions to setup the head section of html documents.</p>
 */
object HeadHelper {
  /**
   * This method returns its parameter unmodified.
   */
  def identity(xml: NodeSeq) : NodeSeq = xml

  /**
   * Removes duplicate node but treats <stript> and <link> tags differently. <script> containing the
   * same src attribute and <link> containing the same href attribute value are considered duplicates.
   */
  def removeHtmlDuplicates(in: NodeSeq): NodeSeq = {
    var jsSources = new ListBuffer[NodeSeq]
    var hrefs = new ListBuffer[NodeSeq]

    Text("\n\t") ++ (in flatMap { e =>
       val src = e.attributes("src")
       val href = e.attributes("href")

       e match {
         case e : Elem if (e.label == "script") && (src != null) && (jsSources contains src) => NodeSeq.Empty
         case e : Elem if (e.label == "script") && (src != null) &&  (!(jsSources contains src)) => jsSources += src; e

         case e : Elem if (e.label == "link") && (href != null) && (hrefs contains href) => NodeSeq.Empty
         case e : Elem if (e.label == "link") && (href != null) && !(hrefs contains href)=> hrefs += href; e

         case e: Text if (e.text.trim.length == 0) => NodeSeq.Empty

         case e => e
      }
    }).flatMap(e => e ++ Text("\n\t"))
  }


  /**
   * This method finds all &lt;head&gt; tags that are descendants of
   * &lt;body&gt; tags in the specified NodeSequence and merges
   * the contents of those tags into the &lt;head&gt; tag closest
   * to the root of the XML tree.
   */
  def mergeToHtmlHead(xhtml: NodeSeq) : NodeSeq = {

    val headInBody: NodeSeq =
      for {
	body <- xhtml \ "body";
	head <- findElems(body)(_.label == "head")
	child <- head.child
      } yield child

    if (headInBody.isEmpty) {
      xhtml
    } else {
      def xform(in: NodeSeq, inBody: Boolean): NodeSeq = in flatMap {
        case e: Elem if !inBody && e.label == "body" =>
          Elem(e.prefix, e.label, e.attributes, e.scope, xform(e.child, true) :_*)

        case e: Elem if inBody && e.label == "head" => NodeSeq.Empty

        case e: Elem if e.label == "head" =>
	  val tList = new ListBuffer[Node]
	tList ++= e.child
	tList ++= headInBody
          Elem(e.prefix, e.label, e.attributes,
               e.scope, removeHtmlDuplicates(tList.toList) :_*)

        case e: Elem =>
          Elem(e.prefix, e.label, e.attributes, e.scope, xform(e.child, inBody) :_*)

        case g: Group =>
          xform(g.child, inBody)

        case x => x
      }

      xform(xhtml, false)
    }
  }
}
