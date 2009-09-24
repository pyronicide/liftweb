/*
 * Copyright 2007-2008 WorldWide Conferencing, LLC
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
package sitemap

import _root_.net.liftweb.http._
import _root_.net.liftweb.util._
import Helpers._

import _root_.scala.xml.{NodeSeq, Text}

class SiteMapException(msg: String) extends Exception(msg)

case class SiteMap(globalParamFuncs: List[PartialFunction[Box[Req], Loc.LocParam]], kids: Menu*) extends HasKids  {
  import SiteMap._
  private var locs: Map[String, Loc[_]] = Map.empty

  kids.foreach(_._parent = Full(this))
  kids.foreach(_.init(this))
  kids.foreach(_.validate)
  private[sitemap] def addLoc(in: Loc[_]) {
    val name = in.name
    if (locs.isDefinedAt(name))
    throw new SiteMapException("Location "+name+" defined twice "+
                               locs(name)+" and "+in)
    else locs = locs + (name -> in.asInstanceOf[Loc[_]])
  }

  def globalParams: List[Loc.LocParam] = {
    val r = S.request

    globalParamFuncs.flatMap(f => if (f.isDefinedAt(r)) List(f(r)) else Nil)
  }

  def findLoc(name: String): Box[Loc[_]] =
  Box(locs.get(name))

  def findLoc(req: Req): Box[Loc[_]] =
  first(kids)(_.findLoc(req))

  def locForGroup(group: String): Seq[Loc[_]] =
  kids.flatMap(_.locForGroup(group)).filter(_.testAccess match {
      case Left(true) => true case _ => false})

  lazy val menus: List[Menu] = locs.valuesIterator.map(_.menu).toList

  def buildMenu(current: Box[Loc[_]]): CompleteMenu = {
    val path: List[Loc[_]] = current match {
      case Full(loc) => loc.breadCrumbs
      case _ => Nil
    }
    CompleteMenu(kids.flatMap(_.makeMenuItem(path)))
  }
}

object SiteMap {
  def findLoc(name: String): Box[Loc[_]] =
  for (sm <- LiftRules.siteMap;
       loc <- sm.findLoc(name)) yield loc

  def findAndTestLoc(name: String): Box[Loc[_]] =
  findLoc(name).flatMap(l => l.testAccess match {
      case Left(true) => Full(l)
      case _ => Empty
    })

  def buildLink(name: String, text: NodeSeq): NodeSeq =
  (for (loc <- findAndTestLoc(name).toList;
       link <- loc.createDefaultLink)
  yield <a href={link}>{
      text match {
        case x if x.text.length > 0 => x
        case _ => loc.linkText openOr Text(loc.name)
      }
    }</a>).headOption getOrElse NodeSeq.Empty

  def buildLink(name: String): NodeSeq =
  buildLink(name, Nil)

  def apply(kids: Menu *) = new SiteMap(Nil, kids :_*)
}

trait HasKids {
  def kids: Seq[Menu]
  def buildUpperLines(pathAt: HasKids, actual: Menu, populate: List[MenuItem]): List[MenuItem]
  = populate

  def isRoot_? = false

  private[sitemap] def testAccess: Either[Boolean, Box[() => LiftResponse]] = Left(true)
}
