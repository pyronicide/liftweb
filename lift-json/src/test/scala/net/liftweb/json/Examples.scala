package net.liftweb.json

import _root_.org.specs.Specification
import _root_.org.specs.runner.{Runner, JUnit}

class ExampleTest extends Runner(Examples) with JUnit
object Examples extends Specification {
  import JsonAST._
  import JsonDSL._
  import JsonParser._

  "Lotto example" in {
    val json = parse(lotto)
    val renderedLotto = compact(render(json))
    json mustEqual parse(renderedLotto)
  }

  "Person example" in {
    val json = parse(person)
    val renderedPerson = JsonDSL.pretty(render(json))
    json mustEqual parse(renderedPerson)
    render(json) mustEqual render(personDSL)
    compact(render(json \\ "name")) mustEqual """{"name":"Joe","name":"Marilyn"}"""
    compact(render(json \ "person" \ "name")) mustEqual "\"name\":\"Joe\""
  }

  "Queries on person example" in {
    val json = parse(person)
    val filtered = json filter {
      case JField("name", _) => true
      case _ => false
    }
    filtered mustEqual List(JField("name", JString("Joe")), JField("name", JString("Marilyn")))

    val found = json find {
      case JField("name", _) => true
      case _ => false
    }
    found mustEqual Some(JField("name", JString("Joe")))
  }

  "Object array example" in {
    val json = parse(objArray)
    compact(render(json \ "children" \ "name")) mustEqual """["name":"Mary","name":"Mazy"]"""
    compact(render((json \ "children")(0) \ "name")) mustEqual "\"name\":\"Mary\""
    compact(render((json \ "children")(1) \ "name")) mustEqual "\"name\":\"Mazy\""
    (for { JField("name", JString(y)) <- json } yield y) mustEqual List("joe", "Mary", "Mazy")
  }

  "Quoted example" in {
    val json = parse(quoted)
    List("foo \" \n \t \r bar") mustEqual json.values
  }

  "Null example" in {
    compact(render(parse(""" {"name": null} """))) mustEqual """{"name":null}"""
  }

  "Null rendering example" in {
    compact(render(nulls)) mustEqual """{"f1":null,"f2":[null,"s"]}"""
  }

  "Unicode example" in {
    parse("[\" \\u00e4\\u00e4li\\u00f6t\"]") mustEqual JArray(List(JString(" \u00e4\u00e4li\u00f6t")))
  }

  "Exponent example" in {
    parse("""{"num": 2e5 }""") mustEqual JObject(List(JField("num", JDouble(200000.0))))
    parse("""{"num": -2E5 }""") mustEqual JObject(List(JField("num", JDouble(-200000.0))))
    parse("""{"num": 2.5e5 }""") mustEqual JObject(List(JField("num", JDouble(250000.0))))
    parse("""{"num": 2.5e-5 }""") mustEqual JObject(List(JField("num", JDouble(2.5e-5))))
  }

  val lotto = """
{
  "lotto":{
    "lotto-id":5,
    "winning-numbers":[2,45,34,23,7,5,3],
    "winners":[ {
      "winner-id":23,
      "numbers":[2,45,34,23,3, 5]
    },{
      "winner-id" : 54 ,
      "numbers":[ 52,3, 12,11,18,22 ]
    }]
  }
}
"""

  val person = """
{ 
  "person": {
    "name": "Joe",
    "age": 35,
    "spouse": {
      "person": {
        "name": "Marilyn",
        "age": 33
      }
    }
  }
}
"""

  val personDSL = 
    ("person" ->
      ("name" -> "Joe") ~
      ("age" -> 35) ~
      ("spouse" -> 
        ("person" -> 
          ("name" -> "Marilyn") ~
          ("age" -> 33)
        )
      )
    )

  val objArray = 
"""
{ "name": "joe",
  "address": {
    "street": "Bulevard",
    "city": "Helsinki"
  },
  "children": [
    {
      "name": "Mary",
      "age": 5
    },
    {
      "name": "Mazy",
      "age": 3
    }
  ]
}
"""

  val nulls = ("f1" -> null) ~ ("f2" -> List(null, "s"))
  val quoted = """["foo \" \n \t \r bar"]"""
}
