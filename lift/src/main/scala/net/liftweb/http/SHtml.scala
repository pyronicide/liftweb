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

import S._
import _root_.net.liftweb.util._
import _root_.net.liftweb.util.Helpers._
import _root_.net.liftweb.http.js._
import _root_.net.liftweb.http.js.AjaxInfo
import JE._
import JsCmds._
import _root_.scala.xml._

object SHtml {

  /**
   * Invokes the Ajax request
   * @param in -- the JsExp that returns the request data
   */
  def makeAjaxCall(in: JsExp): JsExp = new JsExp {
    def toJsCmd = "liftAjax.lift_ajaxHandler("+ in.toJsCmd+", null, null, null)"
  }

  /**
   * Invokes the Ajax request
   * @param in -- the JsExp that returns the request data
   * @context -- defined the response callback functions and the response type (JavaScript or JSON)
   */
  def makeAjaxCall(in: JsExp, context: AjaxContext): JsExp = new JsExp {
    def toJsCmd = "liftAjax.lift_ajaxHandler("+ in.toJsCmd+", " + (context.success openOr "null") +
    ", " + (context.failure openOr "null") +
    ", " + context.responseType.toString.encJs +
    ")"
  }

  /**
   * Build a JavaScript function that will perform an AJAX call based on a value calculated in JavaScript
   * @param jsCalcValue -- the JavaScript to calculate the value to be sent to the server
   * @param func -- the function to call when the data is sent
   *
   * @return the JavaScript that makes the call
   */
  def ajaxCall(jsCalcValue: JsExp, func: String => JsCmd): (String, JsExp) = ajaxCall_*(jsCalcValue, SFuncHolder(func))

  def ajaxCall(jsCalcValue: JsExp, jsContext: JsContext, func: String => JsCmd): (String, JsExp) = 
  ajaxCall_*(jsCalcValue, jsContext, SFuncHolder(func))

  /**
   * Build a JavaScript function that will perform an AJAX call based on a value calculated in JavaScript
   * @param jsCalcValue -- the JavaScript to calculate the value to be sent to the server
   * @param func -- the function to call when the data is sent
   *
   * @return the JavaScript that makes the call
   */
  def jsonCall(jsCalcValue: JsExp, func: Any => JsCmd): (String, JsExp) =
  jsonCall_*(jsCalcValue, SFuncHolder(s => JSONParser.parse(s).map(func) openOr Noop))

  def jsonCall(jsCalcValue: JsExp, jsContext: JsContext, func: Any => JsCmd): (String, JsExp) =
  jsonCall_*(jsCalcValue, jsContext, SFuncHolder(s => JSONParser.parse(s).map(func) openOr Noop))


  /**
   * Build a JavaScript function that will perform an AJAX call based on a value calculated in JavaScript
   * @param jsCalcValue -- the JavaScript to calculate the value to be sent to the server
   * @param func -- the function to call when the data is sent
   *
   * @return the JavaScript that makes the call
   */
  private def jsonCall_*(jsCalcValue: JsExp, func: AFuncHolder): (String, JsExp) =
  fmapFunc(func)(name =>
    (name, makeAjaxCall(JsRaw("'"+name+"=' + encodeURIComponent(JSON.stringify("+jsCalcValue.toJsCmd+"))"))))

  /**
   * Build a JavaScript function that will perform an AJAX call based on a value calculated in JavaScript
   * @param jsCalcValue -- the JavaScript to calculate the value to be sent to the server
   * @param ajaxContext -- the context defining the javascript callback functions and the response type
   * @param func -- the function to call when the data is sent
   *
   * @return the JavaScript that makes the call
   */
  private def jsonCall_*(jsCalcValue: JsExp,
                         ajaxContext: AjaxContext,
                         func: AFuncHolder): (String, JsExp) =
  fmapFunc(func)(name =>
    (name, makeAjaxCall(JsRaw("'"+name+"=' + encodeURIComponent(JSON.stringify("+jsCalcValue.toJsCmd+"))"), ajaxContext)))

  def fajaxCall[T](jsCalcValue: JsExp, func: String => JsCmd)(f: (String, JsExp) => T): T = {
    val (name, js) = ajaxCall(jsCalcValue, func)
    f(name, js)
  }

  def jsonCall(jsCalcValue: JsExp,
               jsonContext: JsonContext,
               func: String => JsObj): (String, JsExp) = ajaxCall_*(jsCalcValue, jsonContext, SFuncHolder(func))

  def fjsonCall[T](jsCalcValue: JsExp, jsonContext: JsonContext, func: String => JsObj)(f: (String, JsExp) => T): T = {
    val (name, js) = jsonCall(jsCalcValue, jsonContext, func)
    f(name, js)
  }

  /**
   * Build a JavaScript function that will perform an AJAX call based on a value calculated in JavaScript
   * @param jsCalcValue -- the JavaScript to calculate the value to be sent to the server
   * @param func -- the function to call when the data is sent
   *
   * @return the JavaScript that makes the call
   */
  private def ajaxCall_*(jsCalcValue: JsExp, func: AFuncHolder): (String, JsExp) =
  fmapFunc(func)(name =>
    (name, makeAjaxCall(JsRaw("'"+name+"=' + encodeURIComponent("+jsCalcValue.toJsCmd+")"))))

  /**
   * Build a JavaScript function that will perform an AJAX call based on a value calculated in JavaScript
   * @param jsCalcValue -- the JavaScript to calculate the value to be sent to the server
   * @param ajaxContext -- the context defining the javascript callback functions and the response type
   * @param func -- the function to call when the data is sent
   *
   * @return the JavaScript that makes the call
   */
  private def ajaxCall_*(jsCalcValue: JsExp,
                         ajaxContext: AjaxContext,
                         func: AFuncHolder): (String, JsExp) =
  fmapFunc(func)(name =>
    (name, makeAjaxCall(JsRaw("'"+name+"=' + encodeURIComponent("+jsCalcValue.toJsCmd+")"), ajaxContext)))


  private def deferCall(data: JsExp, jsFunc: Call): Call =
  Call(jsFunc.function, (jsFunc.params ++ List(AnonFunc(makeAjaxCall(data)))):_*)

  /**
   * Create an Ajax button. When it's pressed, the function is executed
   *
   * @param text -- the name/text of the button
   * @param func -- the function to execute when the button is pushed.  Return Noop if nothing changes on the browser.
   * @param attrs -- the list of node attributes
   *
   * @return a button to put on your page
   */
  def ajaxButton(text: NodeSeq, func: () => JsCmd, attrs: (String, String)*): Elem = {
    attrs.foldLeft(fmapFunc(func)(name =>
        <button onclick={makeAjaxCall(Str(name+"=true")).toJsCmd+
                         "; return false;"}>{text}</button>))(_ % _)
  }

  /**
   * Create an Ajax buttun that when it's pressed it submits an Ajax request and expects back a JSON
   * construct which will be passed to the <i>success</i> function
   *
   * @param text -- the name/text of the button
   * @param func -- the function to execute when the button is pushed.  Return Noop if nothing changes on the browser.
   * @param ajaxContext -- defines the callback functions and the JSON response type
   * @param attrs -- the list of node attributes
   *
   * @return a button to put on your page
   *
   */
  def jsonButton(text: NodeSeq, func: () => JsObj, ajaxContext: JsonContext, attrs: (String, String)*): Elem = {
    attrs.foldLeft(fmapFunc(func)(name =>
        <button onclick={makeAjaxCall(Str(name+"=true"), ajaxContext).toJsCmd+
                         "; return false;"}>{text}</button>))(_ % _)
  }

  /**
   * Create an Ajax button. When it's pressed, the function is executed
   *
   * @param text -- the name/text of the button
   * @param func -- the function to execute when the button is pushed.  Return Noop if nothing changes on the browser.
   * @param attrs -- the list of node attributes
   *
   * @return a button to put on your page
   */
  def ajaxButton(text: NodeSeq, jsExp: JsExp, func: String => JsCmd, attrs: (String, String)*): Elem = {
    attrs.foldLeft(fmapFunc(SFuncHolder(func))(name =>
        <button onclick={makeAjaxCall(JsRaw(name.encJs+"+'='+encodeURIComponent("+jsExp.toJsCmd+")")).toJsCmd+
                         "; return false;"}>{text}</button>))(_ % _)
  }

  /**
   * Create an Ajax buttun that when it's pressed it submits an Ajax request and expects back a JSON
   * construct which will be passed to the <i>success</i> function
   *
   * @param text -- the name/text of the button
   * @param func -- the function to execute when the button is pushed.  Return Noop if nothing changes on the browser.
   * @param ajaxContext -- defines the callback functions and the JSON response type
   * @param attrs -- the list of node attributes
   *
   * @return a button to put on your page
   *
   */
  def jsonButton(text: NodeSeq, jsExp: JsExp, func: Any => JsObj, ajaxContext: JsonContext, attrs: (String, String)*): Elem = {
    attrs.foldLeft(jsonFmapFunc(func)(name =>
        <button onclick={makeAjaxCall(JsRaw(name.encJs+"+'='+ encodeURIComponent(JSON.stringify("+jsExp.toJsCmd+"))"), ajaxContext).toJsCmd+
                         "; return false;"}>{text}</button>))(_ % _)
  }

  /**
   * Create an Ajax button. When it's pressed, the function is executed
   *
   * @param text -- the name/text of the button
   * @param jsFunc -- the user function that will be executed. This function will receive as last parameter
   *                  the function that will actually do the ajax call. Hence the user function can decide when
   * 				  to make the ajax request.
   * @param func -- the function to execute when the button is pushed.  Return Noop if nothing changes on the browser.
   *
   * @return a button to put on your pagejsFunc.params ++ List(AnonFunc(makeAjaxCall(Str(name+"=true"))))
   */
  def ajaxButton(text: NodeSeq, jsFunc: Call, func: () => JsCmd, attrs: (String, String)*): Elem = {
    attrs.foldLeft(fmapFunc(func)(name =>
        <button onclick={deferCall(Str(name+"=true"), jsFunc).toJsCmd + "; return false;"}>{text}</button>))(_ % _)
  }

  /**
   * Create an Ajax button. When it's pressed, the function is executed
   *
   * @param text -- the name/text of the button
   * @param jsFunc -- the user function that will be executed. This function will receive as last parameter
   *                  the function that will actually do the ajax call. Hence the user function can decide when
   * 				  to make the ajax request.
   * @param func -- the function to execute when the button is pushed.  Return Noop if nothing changes on the browser.
   *
   * @return a button to put on your page
   */
  def ajaxButton(text: String, func: () => JsCmd, attrs: (String, String)*): Elem =
  ajaxButton(Text(text), func, attrs :_*)

  /**
   * Create an Ajax button. When it's pressed, the function is executed
   *
   * @param text -- the name/text of the button
   * @param func -- the function to execute when the button is pushed.  Return Noop if nothing changes on the browser.
   *
   * @return a button to put on your page
   */
  def ajaxButton(text: String, jsFunc: Call, func: () => JsCmd, attrs: (String, String)*): Elem =
  ajaxButton(Text(text), jsFunc, func, attrs :_*)


  /**
   * Create an anchor tag around a body which will do an AJAX call and invoke the function
   *
   * @param func - the function to invoke when the link is clicked
   * @param body - the NodeSeq to wrap in the anchor tag
   * @param attrs - the anchor node attributes
   */
  def a(func: () => JsCmd, body: NodeSeq, attrs: (String, String)*): Elem = {
    val key = formFuncName
    addFunctionMap(key, (a: List[String]) => func())
    attrs.foldLeft(<lift:a key={key}>{body}</lift:a>)(_ % _)
  }

  /**
   * Create an anchor tag around a body which will do an AJAX call and invoke the function
   *
   * @param jsFunc -- the user function that will be executed. This function will receive as last parameter
   *                  the function that will actually do the ajax call. Hence the user function can decide when
   * 				  to make the ajax request.
   * @param func - the function to invoke when the link is clicked
   * @param body - the NodeSeq to wrap in the anchor tag
   * @param attrs - the anchor node attributes
   */
  def a(jsFunc: Call, func: () => JsCmd, body: NodeSeq, attrs: (String, String)*): Elem = {
    attrs.foldLeft(fmapFunc(func)(name =>
        <a href="javascript://" onclick={deferCall(Str(name+"=true"), jsFunc).toJsCmd + "; return false;"}>{body}</a>))(_ % _)
  }

  def a(func: () => JsObj,
        jsonContext: JsonContext,
        body: NodeSeq,
        attrs: (String, String)*): Elem = {

    attrs.foldLeft(fmapFunc(func)(name =>
        <a href="javascript://" onclick={makeAjaxCall(Str(name+"=true"), jsonContext).toJsCmd+"; return false;"}>{body}</a>))(_ % _)
  }

  /**
   * Create an anchor with a body and the function to be executed when the anchor is clicked
   */
  def a(body: NodeSeq, attrs: (String, String)*)(func: => JsCmd): Elem =
  a(() => func, body, attrs :_*)

  /**
   * Create an anchor with a body and the function to be executed when the anchor is clicked
   * @param jsFunc -- the user function that will be executed. This function will receive as last parameter
   *                  the function that will actually do the ajax call. Hence the user function can decide when
   * 				  to make the ajax request.
   * @param body - the NodeSeq to wrap in the anchor tag
   * @param attrs - the anchor node attributes
   */
  def a(jsFunc: Call, body: NodeSeq, attrs: (String, String)*)(func: => JsCmd): Elem =
  a(jsFunc, () => func, body, attrs :_*)

  /**
   * Create an anchor that will run a JavaScript command when clicked
   */
  def a(body: NodeSeq, cmd: JsCmd, attrs: (String, String)*): Elem =
  attrs.foldLeft(<a href="javascript://"
      onclick={cmd.toJsCmd + "; return false;"}>{body}</a>)(_ % _)

  /**
   * Create a span that will run a JavaScript command when clicked
   */
  def span(body: NodeSeq, cmd: JsCmd, attrs: (String, String)*): Elem =
  attrs.foldLeft(<span onclick={cmd.toJsCmd}>{body}</span>)(_ % _)


  def toggleKids(head: Elem, visible: Boolean, func: () => Any, kids: Elem): NodeSeq = {
    fmapFunc(func){
      funcName =>

      val (nk, id) = findOrAddId(kids)
      val rnk = if (visible) nk else nk % ("style" -> "display: none")
      val nh = head %
      ("onclick" -> (LiftRules.jsArtifacts.toggle(id).cmd & makeAjaxCall(JsRaw("'"+funcName+"=true'")).cmd))
      nh ++ rnk
    }
  }

  /**
   * This function does not really submit a JSON request to server instead json is a function
   * that allows you to build a more complex JsCmd based on the JsExp <i>JE.JsRaw("this.value")</i>.
   * This function is called by the overloaded version of jsonText.
   *
   * @param value - the initial value of the text field
   * @param json - takes a JsExp which describes how to recover the
   * value of the text field and returns a JsExp containing the thing
   * to execute on blur/return
   *
   * @return a text field
   */
  def jsonText(value: String, json: JsExp => JsCmd, attrs: (String, String)*): Elem = {
    (attrs.foldLeft(<input type="text" value={value}/>)(_ % _)) %
    ("onkeypress" -> """liftUtils.lift_blurIfReturn(event)""") %
    ("onblur" -> (json(JE.JsRaw("this.value"))))
  }

  /**
   * Create a JSON text widget that makes a JSON call on blur or "return".
   *
   * @param value - the initial value of the text field
   * @param cmd - the json command name
   * @param json - the JsonCall returned from S.buildJsonFunc
   *
   * @return a text field
   */
  def jsonText(value: String, cmd: String, json: JsonCall, attrs: (String, String)*): Elem =
  jsonText(value, exp => json(cmd, exp), attrs :_*)

  def ajaxText(value: String, func: String => JsCmd, attrs: (String, String)*): Elem = ajaxText_*(value, Empty, SFuncHolder(func), attrs :_*)

  def ajaxText(value: String, jsFunc: Call, func: String => JsCmd, attrs: (String, String)*): Elem = ajaxText_*(value, Full(jsFunc), SFuncHolder(func), attrs :_*)

  private def ajaxText_*(value: String, jsFunc: Box[Call], func: AFuncHolder, attrs: (String, String)*): Elem = {
    val raw = (funcName: String, value:String) => JsRaw("'" +funcName + "=' + encodeURIComponent(" + value + ".value)")
    val key = formFuncName

    fmapFunc(func){
      funcName =>
      (attrs.foldLeft(<input type="text" value={value}/>)(_ % _)) %
      ("onkeypress" -> """liftUtils.lift_blurIfReturn(event)""") %
      ("onblur" -> (jsFunc match {
            case Full(f) => JsCrVar(key, JsRaw("this")) & deferCall(raw(funcName, key), f)
            case _ => makeAjaxCall(raw(funcName, "this"))
          })
      )
    }
  }

  def ajaxCheckbox(value: Boolean, func: Boolean => JsCmd, attrs: (String, String)*): Elem =
  ajaxCheckbox_*(value, Empty, LFuncHolder(in =>  func(in.exists(toBoolean(_)))), attrs :_*)

  def ajaxCheckbox(value: Boolean, jsFunc: Call, func: Boolean => JsCmd, attrs: (String, String)*): Elem =
  ajaxCheckbox_*(value, Full(jsFunc), LFuncHolder(in =>  func(in.exists(toBoolean(_)))), attrs :_*)

  private def ajaxCheckbox_*(value: Boolean, jsFunc: Box[Call], func: AFuncHolder, attrs: (String, String)*): Elem = {
    val raw = (funcName: String, value:String) => JsRaw("'" +funcName + "=' + " + value + ".checked")
    val key = formFuncName

    fmapFunc(func) {
      funcName =>
      (attrs.foldLeft(<input type="checkbox"/>)(_ % _)) %
      checked(value) %
      ("onclick" -> (jsFunc match {
            case Full(f) => JsCrVar(key, JsRaw("this")) & deferCall(raw(funcName, key), f)
            case _ => makeAjaxCall(raw(funcName, "this"))
          }))
    }
  }


  /**
   * Create a select box based on the list with a default value and the function
   * to be executed on form submission
   *
   * @param options  -- a list of value and text pairs (value, text to display)
   * @param default  -- the default value (or Empty if no default value)
   * @param onSubmit -- the function to execute on form submission
   */
  def ajaxSelectObj[T](options: Seq[(T, String)], default: Box[T],
                       onSubmit: T => JsCmd, attrs: (String, String)*): Elem = {

    val secure = options.map{case (obj, txt) => (obj, randomString(20), txt)}
    val defaultNonce = default.flatMap(d => secure.find(_._1 == d).map(_._2))
    val nonces = secure.map{case (obj, nonce, txt) => (nonce, txt)}
    def process(nonce: String):  JsCmd =
    secure.find(_._2 == nonce).map(x => onSubmit(x._1)) getOrElse Noop
    //  (nonces, defaultNonce, SFuncHolder(process))

    ajaxSelect_*(nonces,
                 defaultNonce,
                 Empty,
                 SFuncHolder(process _),
                 attrs:_*)
  }

  /**
   * Create a select box based on the list with a default value and the function
   * to be executed on form submission
   *
   * @param options  -- a list of value and text pairs (value, text to display)
   * @param default  -- the default value (or Empty if no default value)
   * @param onSubmit -- the function to execute on form submission
   */
  def ajaxSelectObj[T](options: Seq[(T, String)], default: Box[T],
                       jsFunc: Call,
                       onSubmit: T => JsCmd, attrs: (String, String)*): Elem = {

    val secure = options.map{case (obj, txt) => (obj, randomString(20), txt)}
    val defaultNonce = default.flatMap(d => secure.find(_._1 == d).map(_._2))
    val nonces = secure.map{case (obj, nonce, txt) => (nonce, txt)}
    def process(nonce: String):  JsCmd =
    secure.find(_._2 == nonce).map(x => onSubmit(x._1)) getOrElse Noop
    //  (nonces, defaultNonce, SFuncHolder(process))

    ajaxSelect_*(nonces,
                 defaultNonce,
                 Full(jsFunc),
                 SFuncHolder(process _),
                 attrs:_*)
  }

  def ajaxSelect(opts: Seq[(String, String)], deflt: Box[String],
                 func: String => JsCmd, attrs: (String, String)*): Elem =
  ajaxSelect_*(opts, deflt, Empty, SFuncHolder(func), attrs :_*)

  def ajaxSelect(opts: Seq[(String, String)], deflt: Box[String],
                 jsFunc: Call, func: String => JsCmd, attrs: (String, String)*): Elem =
  ajaxSelect_*(opts, deflt, Full(jsFunc), SFuncHolder(func), attrs :_*)

  private def ajaxSelect_*(opts: Seq[(String, String)], deflt: Box[String],
                           jsFunc: Box[Call], func: AFuncHolder, attrs: (String, String)*): Elem = {
    val raw = (funcName: String, value:String) => JsRaw("'" +funcName + "=' + this.options[" + value+ ".selectedIndex].value")
    val key = formFuncName

    val vals = opts.map(_._1)
    val testFunc = LFuncHolder(in => in.filter(v => vals.contains(v)) match {case Nil => false case xs => func(xs)}, func.owner)
    fmapFunc(testFunc) {
      funcName =>
      (attrs.foldLeft(<select>{
              opts.flatMap{case (value, text) => (<option value={value}>{text}</option>) % selected(deflt.exists(_ == value))}
            }</select>)(_ % _)) %
      ("onchange" -> (jsFunc match {
            case Full(f) => JsCrVar(key, JsRaw("this")) & deferCall(raw(funcName, key), f)
            case _ => makeAjaxCall(raw(funcName, "this"))
          }))
    }
  }

  def ajaxInvoke(func: () => JsCmd): (String, JsExp) =
  fmapFunc(NFuncHolder(func))(name => (name, makeAjaxCall(name + "=true")))

  /**
   * Build a swappable visual element.  If the shown element is clicked on, it turns into the hidden element and when
   * the hidden element blurs, it swaps into the shown element.
   */
  def swappable(shown: Elem, hidden: Elem): Elem = {
    val (rs, sid) = findOrAddId(shown)
    val (rh, hid) = findOrAddId(hidden)
    val ui = LiftRules.jsArtifacts
    (<span>{rs % ("onclick" -> (ui.hide(sid).cmd &
                                ui.showAndFocus(hid).cmd & JsRaw("return false;")))}
        {dealWithBlur(rh % ("style" -> "display: none"), (ui.show(sid).cmd & ui.hide(hid).cmd))}
     </span>)
  }

  def swappable(shown: Elem, hidden: String => Elem): Elem = {
    val (rs, sid) = findOrAddId(shown)
    val hid = formFuncName
    val ui = LiftRules.jsArtifacts

    val rh = <span id={hid}>{hidden(ui.show(sid).toJsCmd + ";" + ui.hide(hid).toJsCmd + ";")}</span>
    (<span>{rs % ("onclick" -> (ui.hide(sid).toJsCmd + ";" + ui.show(hid).toJsCmd + "; return false;"))}{
          (rh % ("style" -> "display: none"))}</span>)
  }

  private def dealWithBlur(elem: Elem, blurCmd: String): Elem = {
    (elem \ "@onblur").toList match {
      case Nil => elem % ("onblur" -> blurCmd)
      case x :: xs => val attrs = elem.attributes.filter(_.key != "onblur")
        Elem(elem.prefix, elem.label, new UnprefixedAttribute("onblur", Text(blurCmd + x.text), attrs), elem.scope, elem.child :_*)
    }
  }


  /**
   * create an anchor tag around a body
   *
   * @param func - the function to invoke when the link is clicked
   * @param body - the NodeSeq to wrap in the anchor tag
   */
  def link(to: String, func: () => Any, body: NodeSeq,
           attrs: (String, String)*): Elem = {
    fmapFunc((a: List[String]) => {func(); true})(key =>
      attrs.foldLeft(<a href={to+"?"+key+"=_"}>{body}</a>)(_ % _))
  }

  private def makeFormElement(name: String, func: AFuncHolder,
                              attrs: (String, String)*): Elem =
  fmapFunc(func)(funcName =>
    attrs.foldLeft(<input type={name} name={funcName}/>)(_ % _))

  def text_*(value: String, func: AFuncHolder, attrs: (String, String)*): Elem =
  text_*(value, func, Empty, attrs :_*)

  def text_*(value: String, func: AFuncHolder, ajaxTest: String => JsCmd, attrs: (String, String)*): Elem =
  text_*(value, func, Full(ajaxTest), attrs :_*)

  private def buildOnBlur(bf: Box[String => JsCmd]): MetaData = bf match {
    case Full(func) =>
      new UnprefixedAttribute("onblur", Text(ajaxCall(JsRaw("this.value"),func)._2.toJsCmd), Null) // HERE

    case _ => Null
  }
  
  def text_*(value: String, func: AFuncHolder, ajaxTest: Box[String => JsCmd], attrs: (String, String)*): Elem =
  makeFormElement("text", func, attrs :_*) % new UnprefixedAttribute("value", Text(value), Null) % buildOnBlur(ajaxTest)

  def password_*(value: String, func: AFuncHolder, attrs: (String, String)*): Elem =
  makeFormElement("password", func, attrs :_*) % ("value" -> value)
  def hidden_*(func: AFuncHolder, attrs: (String, String)*): Elem =
  makeFormElement("hidden", func, attrs :_*) % ("value" -> "true")

  def submit_*(value: String, func: AFuncHolder, attrs: (String, String)*): Elem =
  {
    def doit = makeFormElement("submit", func, attrs :_*) % ("value" -> value)

    _formGroup.is match {
      case Empty => formGroup(1)(doit)
      case _ => doit
    }
  }

  def text(value: String, func: String => Any, attrs: (String, String)*): Elem =
  text_*(value, SFuncHolder(func), attrs :_*)

  def textAjaxTest(value: String, func: String => Any, ajaxTest: String => JsCmd, attrs: (String, String)*): Elem =
  text_*(value, SFuncHolder(func), ajaxTest, attrs :_*)

  def textAjaxTest(value: String, func: String => Any, ajaxTest: Box[String => JsCmd], attrs: (String, String)*): Elem =
  text_*(value, SFuncHolder(func), ajaxTest, attrs :_*)


  def password(value: String, func: String => Any, attrs: (String, String)*): Elem =
  makeFormElement("password", SFuncHolder(func), attrs :_*) % new UnprefixedAttribute("value", Text(value), Null)

  def hidden(func: () => Any, attrs: (String, String)*): Elem =
  makeFormElement("hidden", NFuncHolder(func), attrs :_*) % ("value" -> "true")

  def hidden(func: (String) => Any, defaultlValue: String, attrs: (String, String)*): Elem =
  makeFormElement("hidden", SFuncHolder(func), attrs :_*) % ("value" -> defaultlValue)

  def submit(value: String, func: () => Any, attrs: (String, String)*): Elem = {

    def doit = {
      makeFormElement("submit", NFuncHolder(func), attrs :_*) %
      new UnprefixedAttribute("value", Text(value), Null)
    }

    _formGroup.is match {
      case Empty => formGroup(1)(doit)
      case _ => doit
    }
  }

  // support mixin of attribuites from xhtml
  def submitButton(func: () => Any, attrs: (String, String)*): Elem = makeFormElement("submit", NFuncHolder(func), attrs :_*)

  def ajaxForm(body: NodeSeq) = (<lift:form>{body}</lift:form>)
  def ajaxForm(onSubmit: JsCmd, body: NodeSeq) = (<lift:form onsubmit={onSubmit.toJsCmd}>{body}</lift:form>)
  def ajaxForm(body: NodeSeq, onSubmit: JsCmd) = (<lift:form onsubmit={onSubmit.toJsCmd}>{body}</lift:form>)

  def jsonForm(jsonHandler: JsonHandler, body: NodeSeq): NodeSeq = jsonForm(jsonHandler, Noop, body)
  def jsonForm(jsonHandler: JsonHandler, onSubmit: JsCmd, body: NodeSeq): NodeSeq = {
    val id = formFuncName
    <form onsubmit={(onSubmit & jsonHandler.call("processForm", FormToJSON(id)) & JsReturn(false)).toJsCmd} id={id}>
      {body}
    </form>
  }

  private def secureOptions[T](options: Seq[(T, String)], default: Box[T],
                               onSubmit: T => Unit): (Seq[(String, String)], Box[String], AFuncHolder) = {
    val secure = options.map{case (obj, txt) => (obj, randomString(20), txt)}
    val defaultNonce = default.flatMap(d => secure.find(_._1 == d).map(_._2))
    val nonces = secure.map{case (obj, nonce, txt) => (nonce, txt)}
    def process(nonce: String): Unit =
    secure.find(_._2 == nonce).map(x => onSubmit(x._1))
    (nonces, defaultNonce, SFuncHolder(process))
  }

  /**
   * Create a select box based on the list with a default value and the function to be executed on
   * form submission
   *
   * @param opts -- the options.  A list of value and text pairs (value, text to display)
   * @param deflt -- the default value (or Empty if no default value)
   * @param func -- the function to execute on form submission
   */
  def select(opts: Seq[(String, String)], deflt: Box[String], func: String => Any, attrs: (String, String)*): Elem =
  select_*(opts, deflt, SFuncHolder(func), attrs :_*)

  /**
   * Create a select box based on the list with a default value and the function
   * to be executed on form submission
   *
   * @param options  -- a list of value and text pairs (value, text to display)
   * @param default  -- the default value (or Empty if no default value)
   * @param onSubmit -- the function to execute on form submission
   */
  def selectObj[T](options: Seq[(T, String)], default: Box[T],
                   onSubmit: T => Unit, attrs: (String, String)*): Elem = {
    val (nonces, defaultNonce, secureOnSubmit) =
    secureOptions(options, default, onSubmit)

    select_*(nonces, defaultNonce, secureOnSubmit, attrs:_*)
  }

  /**
   * Create a select box based on the list with a default value and the function to be executed on
   * form submission
   *
   * @param opts -- the options.  A list of value and text pairs
   * @param deflt -- the default value (or Empty if no default value)
   * @param func -- the function to execute on form submission
   */
  def select_*(opts: Seq[(String, String)],deflt: Box[String],
               func: AFuncHolder, attrs: (String, String)*): Elem = {
    val vals = opts.map(_._1)
    val testFunc = LFuncHolder(in => in.filter(v => vals.contains(v)) match {case Nil => false case xs => func(xs)}, func.owner)

    attrs.foldLeft(fmapFunc(testFunc)(fn => <select name={fn}>{
            opts.flatMap{case (value, text) => (<option value={value}>{text}</option>) % selected(deflt.exists(_ == value))}
          }</select>))(_ % _)
  }

  /**
   * Create a select box based on the list with a default value and the function to be executed on
   * form submission.  No check is made to see if the resulting value was in the original list.
   * For use with DHTML form updating.
   *
   * @param opts -- the options.  A list of value and text pairs
   * @param deflt -- the default value (or Empty if no default value)
   * @param func -- the function to execute on form submission
   */
  def untrustedSelect(opts: Seq[(String, String)], deflt: Box[String],
                      func: String => Any, attrs: (String, String)*): Elem =
  untrustedSelect_*(opts, deflt, SFuncHolder(func), attrs:_*)

  /**
   * Create a select box based on the list with a default value and the function to be executed on
   * form submission.  No check is made to see if the resulting value was in the original list.
   * For use with DHTML form updating.
   *
   * @param opts -- the options.  A list of value and text pairs
   * @param deflt -- the default value (or Empty if no default value)
   * @param func -- the function to execute on form submission
   */
  def untrustedSelect_*(opts: Seq[(String, String)],deflt: Box[String],
                        func: AFuncHolder, attrs: (String, String)*): Elem =
  fmapFunc(func)(funcName =>
    attrs.foldLeft(<select name={funcName}>{
          opts.flatMap{case (value, text) => (<option value={value}>{text}</option>) % selected(deflt.exists(_ == value))}
        }</select>)(_ % _))


  private def selected(in: Boolean) = if (in) new UnprefixedAttribute("selected", "selected", Null) else Null

  def multiSelect(opts: Seq[(String, String)], deflt: Seq[String],
                  func: List[String] => Any, attrs: (String, String)*): Elem =
  multiSelect_*(opts, deflt, LFuncHolder(func), attrs :_*)

  /**
   * Create a select box based on the list with a default value and the function
   * to be executed on form submission
   *
   * @param options  -- a list of value and text pairs (value, text to display)
   * @param default  -- the default value (or Empty if no default value)
   * @param onSubmit -- the function to execute on form submission
   */
  def multiSelectObj[T](options: Seq[(T, String)], default: Seq[T],
                        onSubmit: List[T] => Unit, attrs: (String, String)*): Elem = {
    val (nonces, defaultNonce, secureOnSubmit) =
    secureMultiOptions(options, default, onSubmit)

    multiSelect_*(nonces, defaultNonce, secureOnSubmit, attrs:_*)
  }

  private[http] def secureMultiOptions[T](options: Seq[(T, String)], default: Seq[T],
                                          onSubmit: List[T] => Unit): (Seq[(String, String)],
                                                                       Seq[String], AFuncHolder) =
  {
    val o2 = options.toList

    val secure: List[(T, String, String)] = o2.map{case (obj, txt) => (obj, randomString(20), txt)}
    val sm: Map[String, T] = Map(secure.map(v => (v._2, v._1)) :_*)
    val defaultNonce: Seq[String] = default.flatMap(d => secure.find(_._1 == d).map(_._2))
    val nonces: List[(String, String)] = secure.map{case (obj, nonce, txt) => (nonce, txt)}.toList
    def process(info: List[String]): Unit = onSubmit(info.flatMap(sm.get) )

    (nonces, defaultNonce, LFuncHolder(process))
  }

  def multiSelect_*(opts: Seq[(String, String)],
                    deflt: Seq[String],
                    func: AFuncHolder, attrs: (String, String)*): Elem =
  fmapFunc(func)(funcName =>
    attrs.foldLeft(<select multiple="true" name={funcName}>{
          opts.flatMap(o => (<option value={o._1}>{o._2}</option>) % selected(deflt.contains(o._1)))
        }</select>)(_ % _))


  def textarea(value: String, func: String => Any, attrs: (String, String)*): Elem =
  textarea_*(value, SFuncHolder(func), attrs :_*)

  def textarea_*(value: String, func: AFuncHolder, attrs: (String, String)*): Elem =
  fmapFunc(func)(funcName =>
    attrs.foldLeft(<textarea name={funcName}>{value}</textarea>)(_ % _))

  def radio(opts: Seq[String], deflt: Box[String], func: String => Any,
            attrs: (String, String)*): ChoiceHolder[String] =
  radio_*(opts, deflt, SFuncHolder(func), attrs :_*)

  def radio_*(opts: Seq[String], deflt: Box[String],
              func: AFuncHolder, attrs: (String, String)*): ChoiceHolder[String] = {
    fmapFunc(func){name =>
      val itemList = opts.map(v => ChoiceItem(v,
                                              attrs.foldLeft(<input type="radio" name={name} value={v}/>)(_ % _) %
                                              checked(deflt.filter((s: String) => s == v).isDefined)))
      ChoiceHolder(itemList)
    }
  }

  def fileUpload(func: FileParamHolder => Unit): Elem = {
    val f2: FileParamHolder => Unit = fp =>
    if (fp.file != null && fp.file.length > 0) func(fp)

    fmapFunc(BinFuncHolder(f2))(name => <input type="file" name={name} />)
  }

  case class ChoiceItem[T](key: T, xhtml: NodeSeq)

  case class ChoiceHolder[T](items: Seq[ChoiceItem[T]]) {
    def apply(in: T) = items.filter(_.key == in).head.xhtml
    def apply(in: Int) = items(in).xhtml
    def map[A](f: ChoiceItem[T] => A) = items.map(f)
    def flatMap[A](f: ChoiceItem[T] => scala.collection.Traversable[A])(implicit bf : scala.collection.generic.BuilderFactory[A, Seq[A], scala.collection.Sequence[ChoiceItem[T]]]): Seq[A] = 
      items.flatMap(f)(bf)
    def filter(f: ChoiceItem[T] => Boolean) = items.filter(f)
    def toForm: NodeSeq = flatMap(ChoiceHolder.htmlize)
  }

object ChoiceHolder {
  var htmlize: ChoiceItem[_] => NodeSeq = c => (<span>{c.xhtml}&nbsp;{c.key.toString}<br /></span>)
}

  private def checked(in: Boolean) = if (in) new UnprefixedAttribute("checked", "checked", Null) else Null
  private def setId(in: Box[String]) = in match { case Full(id) => new UnprefixedAttribute("id", Text(id), Null); case _ => Null}

  def checkbox[T](possible: Seq[T], actual: Seq[T], func: Seq[T] => Any, attrs: (String, String)*): ChoiceHolder[T] = {
    val len = possible.length
    fmapFunc(LFuncHolder( (strl: List[String]) => {func(strl.headOption.toList.map(toInt(_)).filter(x =>x >= 0 && x < len).map(possible(_))); true})){
      name =>


      ChoiceHolder(possible.toList.zipWithIndex.map(p =>
          ChoiceItem(p._1,
                     attrs.foldLeft(<input type="checkbox" name={name} value={p._2.toString}/>)(_ % _) %
                     checked(actual.contains(p._1)) ++ (if (p._2 == 0) (<input type="hidden" name={name} value="-1"/>) else Nil))))
    }
  }

  /**
   * Defines a new checkbox set to {@code value} and running {@code func} when the
   * checkbox is submitted.
   */
  def checkbox(value: Boolean, func: Boolean => Any, attrs: (String, String)*): NodeSeq = {
    checkbox_id(value, func, Empty, attrs :_*)
  }

  /**
   * Defines a new checkbox set to {@code value} and running {@code func} when the
   * checkbox is submitted. Has an id of {@code id}.
   */
  def checkbox_id(value: Boolean, func: Boolean => Any,
                  id: Box[String], attrs: (String, String)*): NodeSeq = {
    def from(f: Boolean => Any): List[String] => Boolean = (in: List[String]) => {
      f(in.exists(toBoolean(_)))
      true
    }
    checkbox_*(value, LFuncHolder(from(func)), id, attrs :_*)
  }

  def checkbox_*(value: Boolean, func: AFuncHolder, id: Box[String],
                 attrs: (String, String)*): NodeSeq = {
    fmapFunc(func)(name =>
      (<input type="hidden" name={name} value="false"/>) ++
      (attrs.foldLeft(<input type="checkbox" name={name} value="true" />)(_ % _) % checked(value) % setId(id))
    )
  }

}

object AjaxType extends Enumeration("javascript", "json") {
  val JavaScript, JSON = Value
}

object AjaxContext {
  def js(success: Box[String], failure: Box[String]) = new JsContext(success, failure)
  def js(success: Box[String]) = new JsContext(success, Empty)

  def json(success: Box[String], failure: Box[String]) = new JsonContext(success, failure)
  def json(success: Box[String]) = new JsonContext(success, Empty)
}


class AjaxContext(val success: Box[String],val failure: Box[String],val responseType: AjaxType.Value)

class JsContext(success: Box[String], failure: Box[String]) extends AjaxContext(success, failure, AjaxType.JavaScript)

class JsonContext(success: Box[String], failure: Box[String]) extends AjaxContext(success, failure, AjaxType.JSON)

