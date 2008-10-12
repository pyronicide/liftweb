/*
 * Copyright 2007-2008 WorldWide Conferencing, LLC
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

package net.liftweb.record.field

import scala.xml._
import net.liftweb.util._
import Helpers._
import net.liftweb.http.{S}
import S._

class DoubleField[OwnerType <: Record[OwnerType]](rec: OwnerType) extends Field[Double, OwnerType] {

  override def owner = rec

  private def toDouble(in: Any): Double = {
    in match {
      case null => 0.0
      case i: Int => i
      case n: Long => n
      case n : Number => n.doubleValue
      case (n: Number) :: _ => n.doubleValue
      case Some(n) => toDouble(n)
      case None => 0.0
      case s: String => s.toDouble
      case x :: xs => toDouble(x)
      case o => toDouble(o.toString)
	}
  }

  /**
   * Sets the field value from an Any
   */
  override def setFromAny(in: Any): Double = {
    in match {
      case n: Double => this.set(n)
      case n: Number => this.set(n.doubleValue)
      case (n: Number) :: _ => this.set(n.doubleValue)
      case Some(n: Number) => this.set(n.doubleValue)
      case None => this.set(0.0)
      case (s: String) :: _ => this.set(toDouble(s))
      case null => this.set(0L)
      case s: String => this.set(toDouble(s))
      case o => this.set(toDouble(o))
    }
  }

  /**
   * Returns form input of this field
   */
  override def toForm = <input type="text" name={S.mapFunc({s: List[String] => this.setFromAny(s)})}
	 value={value.toString}/>

  override def defaultValue = 0.0

}

import java.sql.{ResultSet, Types}
import net.liftweb.mapper.{DriverType}

/**
 * An int field holding DB related logic
 */
abstract class DBDoubleField[OwnerType <: DBRecord[OwnerType]](rec: OwnerType) extends DoubleField[OwnerType](rec) {

  def targetSQLType = Types.DOUBLE

  /**
   * Given the driver type, return the string required to create the column in the database
   */
  def fieldCreatorString(dbType: DriverType, colName: String): String = colName + " " + dbType.enumColumnType

  def jdbcFriendly(field : String) : Double = value

}
