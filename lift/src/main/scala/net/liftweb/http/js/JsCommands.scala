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
package http
package js

import _root_.scala.xml.{NodeSeq, Group, Unparsed, Elem}
import _root_.net.liftweb.util.Helpers._
import _root_.net.liftweb.util.Helpers
import _root_.net.liftweb.util.TimeHelpers
import _root_.net.liftweb.util._
import _root_.scala.xml.{Node, SpecialNode, Text}

object JsCommands {
  def create = new JsCommands(Nil)
  def apply(in: Seq[JsCmd]) = new JsCommands(in.toList.reverse)
  def apply(in: JsExp) = new JsCommands(List(in.cmd))
}

class JsCommands(val reverseList: List[JsCmd]) {
  def &(in: JsCmd) = new JsCommands(in :: reverseList)
  def &(in: List[JsCmd]) = new JsCommands(in.reverse ::: reverseList)

  def toResponse = {
    val data = reverseList.reverse.map(_.toJsCmd).mkString("\n").getBytes("UTF-8")
    InMemoryResponse(data, List("Content-Length" -> data.length.toString, "Content-Type" -> "text/javascript; charset=utf-8"), Nil, 200)
  }
}

case class JsonCall(funcId: String) {
  def exp(exp: JsExp): JsCmd = JsCmds.Run(funcId+"("+exp.toJsCmd+");")

  def apply(command: String): JsCmd = apply(JE.Str(command))

  def apply(command: JsExp): JsCmd =
  JsCmds.Run(funcId+"({'command': "+command.toJsCmd+", 'params': false});")

  def apply(command: String, params: JsExp) =
  JsCmds.Run(funcId+"({'command': "+command.encJs+", 'params':"+
             params.toJsCmd+"});")

  def apply(command: String, target: String, params: JsExp) =
  JsCmds.Run(funcId+"({'command': "+command.encJs+", 'target': "+
             target.encJs+
             ", 'params':"+
             params.toJsCmd+"});")


  def apply(command: JsExp, params: JsExp) =
  JsCmds.Run(funcId+"({'command': "+command.toJsCmd+", 'params':"+
             params.toJsCmd+"});")

  def apply(command: JsExp, target: JsExp, params: JsExp) =
  JsCmds.Run(funcId+"({'command': "+command.toJsCmd+", 'target': "+
             target.toJsCmd+
             ", 'params':"+
             params.toJsCmd+"});")

}


trait JsObj extends JsExp {
  def props: List[(String,JsExp)]
  def toJsCmd = props.map{case (n, v) => n.encJs+": "+v.toJsCmd}.mkString("{", ", ", "}")
  def +*(other: JsObj) = {
    val np = props ::: other.props
    new JsObj {
      def props = np
    }
  }
}

trait JsExp extends SpecialNode with HtmlFixer with JxBase with ToJsCmd {
  def toJsCmd: String

  // def label: String = "#JS"

  override def buildString(sb: StringBuilder) = {
    (new Text(toJsCmd)).buildString(sb)
  }

  def appendToParent(parentName: String): JsCmd = {
    val ran = "v"+Helpers.nextFuncName
    JsCmds.JsCrVar(ran, this) &
    JE.JsRaw("if ("+ran+".parentNode) "+ran+" = "+ran+".cloneNode(true)").cmd &
    JE.JsRaw("if ("+ran+".nodeType) {"+parentName+".appendChild("+ran+");} else {"+
             parentName+".appendChild(document.createTextNode("+ran+"));}").cmd
  }

  /**
   * ~> accesses a property in the current JsExp
   */
  def ~>(right: JsMethod): JsExp =  new JsExp {
    def toJsCmd = JsExp.this.toJsCmd + "." + right.toJsCmd
  }

  def cmd: JsCmd = JsCmds.Run(toJsCmd+";")


  def +(right: JsExp): JsExp = new JsExp {
    def toJsCmd = JsExp.this.toJsCmd + " + "+ right.toJsCmd
  }

  def ===(right: JsExp): JsExp = new JsExp {
    def toJsCmd = JsExp.this.toJsCmd + " = "+ right.toJsCmd
  }

}

trait JsMethod {
  def toJsCmd: String
}

/**
 * JavaScript Expressions. To see these in action, check out
 * sites/example/src/webapp/json.html
 */
object JE {
  implicit def strToS(in: String): Str = Str(in)
  implicit def boolToJsExp(in: Boolean): JsExp = if (in) JsTrue else JsFalse
  implicit def numToJsExp(in: Int): JsExp = Num(in)
  implicit def numToJsExp(in: Long): JsExp = Num(in)
  implicit def numToJsExp(in: Double): JsExp = Num(in)
  implicit def numToJsExp(in: Float): JsExp = Num(in)

  case class Num(n: Number) extends JsExp {
    def toJsCmd = n.toString
  }

  case class Stringify(in: JsExp) extends JsExp {
    def toJsCmd = "JSON.stringify("+in.toJsCmd+")"
  }

  case class JsArray(in: JsExp*) extends JsExp {
    def toJsCmd = new JsExp {
      def toJsCmd = in.map(_.toJsCmd).mkString("[",", ", "]\n")
    }.toJsCmd

    def this(in: List[JsExp]) = this(in :_*)
  }

  case class ValById(id: String) extends JsExp {
    def toJsCmd = "document.getElementById("+id.encJs+").value"
  }

  /**
   * gets the element by ID
   */
  case class ElemById(id: String, then: String*) extends JsExp {
    override def toJsCmd = "document.getElementById("+id.encJs+")" + (
      if (then.isEmpty) "" else then.mkString(".", ".", "")
    )
  }

  object LjSwappable {
    def apply(visible: JsExp, hidden: JsExp): JxBase = {
      new JxNodeBase {
        def child = Nil
        def appendToParent(name: String): JsCmd =
        JsRaw(name+".appendChild(lift$.swappable("+visible.toJsCmd
              +", "+hidden.toJsCmd +"))").cmd
      }
    }

    def apply(visible: NodeSeq, hidden: NodeSeq): JxBase = {
      new JxNodeBase {
        def child = Nil
        def appendToParent(name: String): JsCmd =
        JsRaw(name+".appendChild(lift$.swappable("+AnonFunc(
            JsCmds.JsCrVar("df", JsRaw("document.createDocumentFragment()")) &
            addToDocFrag("df", visible.toList) &
            JE.JsRaw("return df").cmd
          ).toJsCmd
              +"(), "+AnonFunc(JsCmds.JsCrVar("df", JsRaw("document.createDocumentFragment()")) &
                               addToDocFrag("df", hidden.toList) &
                               JE.JsRaw("return df").cmd).toJsCmd +"()))").cmd
      }
    }
  }

  object LjBuildIndex {
    def apply(obj: String,
              indexName: String, tables: (String, String)*): JsExp = new JsExp {
      def toJsCmd = "lift$.buildIndex("+obj+", "+indexName.encJs+
      (if (tables.isEmpty) "" else ", "+
       tables.map{case (l, r) => "["+l.encJs+", "+r.encJs+"]"}.mkString(", "))+
      ")"
    }

    def apply(obj: JsExp,
              indexName: String, tables: (String, String)*): JsExp = new JsExp {
      def toJsCmd = "lift$.buildIndex("+obj.toJsCmd+", "+indexName.encJs+
      (if (tables.isEmpty) "" else ", "+
       tables.map{case (l, r) => "["+l.encJs+", "+r.encJs+"]"}.mkString(", "))+
      ")"
    }
  }

  protected trait MostLjFuncs {
    def funcName: String

    def apply(obj: String, func: String): JsExp = new JsExp {
      def toJsCmd = "lift$."+funcName+"("+obj+", "+func.encJs+")"
    }

    def apply(obj: JsExp, func: JsExp): JsExp = new JsExp {
      def toJsCmd = "lift$."+funcName+"("+obj.toJsCmd+", "+func.toJsCmd+")"
    }
  }

  object LjAlt {

    def apply(obj: String, func: String, alt: String): JsExp = new JsExp {
      def toJsCmd = "lift$.alt("+obj+", "+func.encJs+", "+alt.encJs+")"
    }

    def apply(obj: JsExp, func: JsExp, alt: String): JsExp = new JsExp {
      def toJsCmd = "lift$.alt("+obj.toJsCmd+", "+func.toJsCmd+", "+alt.encJs+")"
    }

    def apply(obj: JsExp, func: JsExp, alt: JsExp): JsExp = new JsExp {
      def toJsCmd = "lift$.alt("+obj.toJsCmd+", "+func.toJsCmd+", "+alt.toJsCmd+")"
    }
  }

  object LjMagicUpdate {
    def apply(obj: String, field: String, idField: String, toUpdate: JsExp): JsExp = new JsExp {
      def toJsCmd = "lift$.magicUpdate("+obj+", "+field.encJs+", "+idField.encJs+", "+toUpdate.toJsCmd+")"
    }

    def apply(obj: JsExp, field: String, idField: String, toUpdate: JsExp): JsExp = new JsExp {
      def toJsCmd = "lift$.magicUpdate("+obj.toJsCmd+", "+field.encJs+", "+idField.encJs+", "+toUpdate.toJsCmd+")"
    }
  }

  object LjForeach extends MostLjFuncs {
    def funcName: String = "foreach"
  }

  object LjFilter extends MostLjFuncs {
    def funcName: String = "filter"
  }

  object LjMap extends MostLjFuncs {
    def funcName: String = "map"
  }

  object LjFold {
    def apply(what: JsExp, initVal: JsExp, func: String): JsExp = new JsExp {
      def toJsCmd = "lift$.fold("+what.toJsCmd+", "+initVal.toJsCmd+", "+func.encJs+")"
    }

    def apply(what: JsExp, initVal: JsExp, func: AnonFunc): JsExp = new JsExp {
      def toJsCmd = "lift$.fold("+what.toJsCmd+", "+initVal.toJsCmd+", "+func.toJsCmd+")"
    }
  }

  object LjFlatMap extends MostLjFuncs {
    def funcName: String = "flatMap"
  }

  object LjSort extends MostLjFuncs {
    def funcName: String = "sort"

    def apply(obj: String): JsExp = new JsExp {
      def toJsCmd = "lift$."+funcName+"("+obj+")"
    }

    def apply(obj: JsExp): JsExp = new JsExp {
      def toJsCmd = "lift$."+funcName+"("+obj.toJsCmd+")"
    }
  }

  object FormToJSON {
    def apply(formId: String) =  new JsExp {
      def toJsCmd = LiftRules.jsArtifacts.formToJSON(formId).toJsCmd;
    }
  }

  /**
   * A String (JavaScript encoded)
   */
  case class Str(str: String) extends JsExp {
    def toJsCmd = str.encJs
  }

  /**
   * A JavaScript method that takes parameters
   */
  case class JsFunc(method: String, params: JsExp*) extends JsMethod {
    def toJsCmd = params.map(_.toJsCmd).mkString(method+"(", ", ", ")")
    def cmd: JsCmd = JsCmds.Run(toJsCmd+";")
  }

  /**
   * Put any JavaScript expression you want in here and the result will be
   * evaluated.
   */
  case class JsRaw(rawJsCmd: String) extends JsExp {
    def toJsCmd = rawJsCmd
  }

  case class JsVar(varName: String, andThen: String*) extends JsExp {
    def toJsCmd = varName + (if (andThen.isEmpty) ""
                             else andThen.mkString(".", ".", ""))
  }

  /**
   * A value that can be retrieved from an expression
   */
  case class JsVal(valueName: String) extends JsMethod {
    def toJsCmd = valueName
  }

  case object Id extends JsMethod {
    def toJsCmd = "id"
  }

  case object Style extends JsMethod {
    def toJsCmd = "style"
  }

  case object Value extends JsMethod {
    def toJsCmd = "value"
  }

  case object JsFalse extends JsExp {
    def toJsCmd = "false"
  }

  case object JsNull extends JsExp {
    def toJsCmd = "null"
  }

  case object JsTrue extends JsExp {
    def toJsCmd = "true"
  }

  case class Call(function: String, params: JsExp*) extends JsExp {
    def toJsCmd = function+"("+params.map(_.toJsCmd).mkString(",")+")"
  }

  trait AnonFunc extends JsExp {
    def applied: JsExp = new JsExp {
      def toJsCmd = "("+AnonFunc.this.toJsCmd + ")" + "()"
    }
    def applied(params: JsExp*): JsExp = new JsExp {
      def toJsCmd = "("+AnonFunc.this.toJsCmd +")" +
      params.map(_.toJsCmd).mkString("(", ",", ")")
    }

  }

  object AnonFunc {
    def apply(in: JsCmd): AnonFunc = new JsExp with AnonFunc {
      def toJsCmd = "function() {"+in.toJsCmd+"}"
    }

    def apply(params: String, in: JsCmd): AnonFunc = new JsExp with AnonFunc {
      def toJsCmd = "function("+params+") {"+in.toJsCmd+"}"
    }
  }

  object JsObj {
    def apply(members: (String, JsExp)*): JsObj =
    new JsObj {
      def props = members.toList
    }
  }

  case class JsLt(left: JsExp, right: JsExp) extends JsExp {
    def toJsCmd = left.toJsCmd + " < " + right.toJsCmd
  }

  case class JsGt(left: JsExp, right: JsExp) extends JsExp {
    def toJsCmd = left.toJsCmd + " > " + right.toJsCmd
  }

  case class JsEq(left: JsExp, right: JsExp) extends JsExp {
    def toJsCmd = left.toJsCmd + " == " + right.toJsCmd
  }

  case class JsNotEQ(left: JsExp, right: JsExp) extends JsExp {
    def toJsCmd = left.toJsCmd + " != " + right.toJsCmd
  }

  case class JsLtEq(left: JsExp, right: JsExp) extends JsExp {
    def toJsCmd = left.toJsCmd + " <= " + right.toJsCmd
  }

  case class JsGtEq(left: JsExp, right: JsExp) extends JsExp {
    def toJsCmd = left.toJsCmd + " >= " + right.toJsCmd
  }

}

trait HtmlFixer {
  def fixHtml(uid: String, content: NodeSeq): String =
  AltXML.toXML(Group(S.session.map(s => s.fixHtml(s.processSurroundAndInclude("JS SetHTML id: "+uid, content))).openOr(content)),
               false, true, S.ieMode).encJs

}

trait JsCmd extends HtmlFixer with ToJsCmd {
  def &(other: JsCmd): JsCmd = JsCmds.CmdPair(this, other)
  def toJsCmd: String
}

object JsCmds {
  implicit def seqJsToJs(in: Seq[JsCmd]): JsCmd = in.foldLeft[JsCmd](Noop)(_ & _)

  object Script {
    def apply(script: JsCmd): Node = <script type="text/javascript">{
        Unparsed("""
// <![CDATA[
"""+  fixEndScriptTag(script.toJsCmd)+ """
// ]]>
""")
      }</script>

    private def fixEndScriptTag(in: String): String =
    if (S.ieMode)  """\<\/script\>""".r.replaceAllIn(in, """<\\/script>""")
    else in
  }

  def JsHideId(what: String): JsCmd = LiftRules.jsArtifacts.hide(what).cmd

  def JsShowId(what: String): JsCmd = LiftRules.jsArtifacts.show(what).cmd

  case class SetHtml(uid: String, content: NodeSeq) extends JsCmd {
    def toJsCmd = LiftRules.jsArtifacts.setHtml(uid, content).toJsCmd
  }


  /**
   * Makes the parameter the selected HTML element on load of the page
   *
   * @param in the element that should have focus
   *
   * @return the element and a script that will give the element focus
   */
  object FocusOnLoad {
    def apply(in: Elem): NodeSeq = {
      val (elem, id) = findOrAddId(in)
      elem ++ Script(LiftRules.jsArtifacts.onLoad(Run("document.getElementById("+id.encJs+").focus();")))
    }
  }

  object Function {
    def apply(name: String, params: List[String], body: JsCmd): JsCmd =
    new JsCmd {
      def toJsCmd = "function "+name+"("+
      params.mkString(", ")+""") {
    """+body.toJsCmd+"""
    }
"""
    }
  }

  object OnLoad{
    def apply(what: JsCmd): JsCmd = LiftRules.jsArtifacts.onLoad(what)

  }

  case class SetValById(id: String, right: JsExp) extends JsCmd {
    def toJsCmd = "document.getElementById("+id.encJs+").value = "+
    right.toJsCmd+";"
  }

  case class SetExp(left: JsExp, right: JsExp) extends JsCmd {
    def toJsCmd = left.toJsCmd + " = " + right.toJsCmd + ";"
  }

  case class JsCrVar(name: String, right: JsExp) extends JsCmd {
    def toJsCmd = "var "+name + " = "+right.toJsCmd + ";"
  }

  case class SetElemById(id: String, right: JsExp, then: String*) extends JsCmd {
    def toJsCmd = "document.getElementById("+id.encJs+")"+ (
      if (then.isEmpty) "" else then.mkString(".", ".", "")
    ) + " = "+right.toJsCmd + ";"
  }

  implicit def jsExpToJsCmd(in: JsExp) = in.cmd

  case class CmdPair(left: JsCmd, right: JsCmd) extends JsCmd {
    def toJsCmd = {
      val sb = new StringBuilder
      append(sb, this)
      sb.toString
    }

    private def append(sb: StringBuilder, cmd: JsCmd) {
      cmd match {
        case CmdPair(l, r) => append(sb, l)
          sb.append('\n')
          append(sb, r)

        case c => sb.append(c.toJsCmd)
      }
    }
  }

  trait HasTime {
    def time: Box[TimeSpan]
    def timeStr = time.map(_.millis.toString) openOr ""
  }

  case class After(time: TimeSpan, toDo: JsCmd) extends JsCmd {
    def toJsCmd = "setTimeout(function() {"+toDo.toJsCmd+"}, "+time.millis+");"
  }

  case class Alert(text: String) extends JsCmd {
    def toJsCmd = "alert("+text.encJs+");"
  }

  case class Confirm(text: String, yes: JsCmd) extends JsCmd {
    def toJsCmd = "if (confirm("+text.encJs+")) {"+yes.toJsCmd+"}"
  }

  case class Run(text: String) extends JsCmd {
    def toJsCmd = text
  }

  case object _Noop extends JsCmd {
    def toJsCmd = ""
  }

  implicit def cmdToString(in: JsCmd): String = in.toJsCmd

  val Noop: JsCmd = _Noop

  case class JsTry(what: JsCmd, alert: Boolean) extends JsCmd {
    def toJsCmd = "try { "+what.toJsCmd+" } catch (e) {"+(if (alert) "alert(e);" else "")+"}"
  }

  case class RedirectTo(where: String) extends JsCmd {
    private val context = S.contextPath
    def toJsCmd = "window.location = "+S.encodeURL(context + where).encJs+";"
  }


  /**
   * Update a Select with new Options
   */
  case class ReplaceOptions(select: String, opts: List[(String, String)], dflt: Box[String]) extends JsCmd {
    def toJsCmd = """var x=document.getElementById("""+select.encJs+""");
    while (x.length > 0) {x.remove(0);}
    var y = null;
    """+
    opts.map{case (value, text) =>
        "y=document.createElement('option'); "+
        "y.text = "+text.encJs+"; "+
        "y.value = "+value.encJs+"; "+
        (if (value == dflt) "y.selected = true; " else "") +
        " try {x.add(y, null);} catch(e) {if (typeof(e) == 'object' && typeof(e.number) == 'number' && (e.number & 0xFFFF) == 5){ x.add(y,x.options.length); } } "
    }.mkString("\n")
  }

  case object JsIf {
    def apply(condition: JsExp, body: JsCmd):JsCmd = JE.JsRaw("if ( " + condition.toJsCmd  + " ) { " + body.toJsCmd + " }")

    def apply(condition: JsExp, bodyTrue: JsCmd, bodyFalse: JsCmd) : JsCmd =
    JE.JsRaw("if ( " + condition.toJsCmd  +" ) { " + bodyTrue.toJsCmd + " } else { " + bodyFalse.toJsCmd + " }")

    def apply(condition: JsExp, body: JsExp):JsCmd = JE.JsRaw("if ( " + condition.toJsCmd  + " ) { " + body.toJsCmd + " }")

    def apply(condition: JsExp, bodyTrue: JsExp, bodyFalse: JsExp) : JsCmd =
    JE.JsRaw("if ( " + condition.toJsCmd  +" ) { " + bodyTrue.toJsCmd + " } else { " + bodyFalse.toJsCmd + " }")
  }

  case class JsWhile(condition: JsExp, body: JsExp) extends JsCmd {
    def toJsCmd = "while ( " + condition.toJsCmd + " ) { " + body.toJsCmd + " }"
  }

  case class JsWith(reference: String, body: JsExp) extends JsCmd {
    def toJsCmd = "with ( " + reference + " ) { " + body.toJsCmd + " }"
  }

  case class JsDoWhile(body: JsExp, condition: JsExp) extends JsCmd {
    def toJsCmd = "do { " + body.toJsCmd + " } while ( " + condition.toJsCmd + " )"
  }

  case class JsFor(initialExp: JsExp, condition: JsExp, incrementExp: JsExp, body: JsExp) extends JsCmd {
    def toJsCmd = "for ( " + initialExp.toJsCmd + "; " +
    condition.toJsCmd + "; " +
    incrementExp.toJsCmd + " ) { " + body.toJsCmd + " }"
  }

  case class JsForIn(initialExp: JsExp, reference: String, body: JsCmd) extends JsCmd {
    def toJsCmd = "for ( " + initialExp.toJsCmd + " in " + reference+ ") { " + body.toJsCmd + " }"
  }

  case object JsBreak extends JsCmd {
    def toJsCmd = "break"
  }

  case object JsContinue extends JsCmd {
    def toJsCmd = "continue"
  }

  object JsReturn {
    def apply(in: JsExp): JsCmd = new JsCmd {
      def toJsCmd = "return " + in.toJsCmd
    }
    def apply(): JsCmd =  new JsCmd {
      def toJsCmd = "return "
    }
  }

}



