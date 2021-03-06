package net.liftweb.json

import _root_.org.specs.Specification
import _root_.org.specs.runner.{Runner, JUnit}

class XmlExamplesTest extends Runner(XmlExamples) with JUnit
object XmlExamples extends Specification {
  import JsonAST._
  import JsonDSL._
  import Xml._

  "Basic conversion example" in {
    val json = toJson(users1) 
    compact(render(json)) mustEqual """{"users":{"user":[{"id":"1","name":"Harry"},{"id":"2","name":"David"}]}}"""
  }

  "Conversion transformation example 1" in {
    val json = toJson(users1) map {
      case JField("id", JString(s)) => JField("id", JInt(s.toInt))
      case x => x 
    }
    compact(render(json)) mustEqual """{"users":{"user":[{"id":1,"name":"Harry"},{"id":2,"name":"David"}]}}"""
  }

  "Conversion transformation example 2" in {
    val json = toJson(users2) map {
      case JField("id", JString(s)) => JField("id", JInt(s.toInt))
      case JField("user", x: JObject) => JField("user", JArray(x :: Nil))
      case x => x 
    }
    compact(render(json)) mustEqual """{"users":{"user":[{"id":1,"name":"Harry"}]}}"""
  }

  "Primitive array example" in {
    val xml = <chars><char>a</char><char>b</char><char>c</char></chars>
    compact(render(toJson(xml))) mustEqual """{"chars":{"char":["a","b","c"]}}"""
  }

  "Lotto example which flattens number arrays into encoded string arrays" in {
    def flattenArray(nums: List[JValue]) = JString(nums.map(_.values).mkString(","))

    val printer = new scala.xml.PrettyPrinter(100,2)
    val lotto: JObject = LottoExample.json
    val xml = toXml(lotto map {
      case JField("winning-numbers", JArray(nums)) => JField("winning-numbers", flattenArray(nums))
      case JField("numbers", JArray(nums)) => JField("numbers", flattenArray(nums))
      case x => x
    })

    printer.format(xml(0)) mustEqual printer.format(
      <lotto>
        <id>5</id>
        <winning-numbers>2,45,34,23,7,5,3</winning-numbers>
        <winners>
          <winner-id>23</winner-id>
          <numbers>2,45,34,23,3,5</numbers>
        </winners>
        <winners>
          <winner-id>54</winner-id>
          <numbers>52,3,12,11,18,22</numbers>
        </winners>
      </lotto>)
  }

  val users1 =
    <users>
      <user>
        <id>1</id>
        <name>Harry</name>
      </user>
      <user>
        <id>2</id>
        <name>David</name>
      </user>
    </users>   

  val users2 =
    <users>
      <user>
        <id>1</id>
        <name>Harry</name>
      </user>
    </users>   
}
