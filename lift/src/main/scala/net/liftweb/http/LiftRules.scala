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

import _root_.net.liftweb.util._
import _root_.net.liftweb.util.Helpers._
import _root_.net.liftweb.sitemap._
import _root_.net.liftweb.http.js.JSArtifacts
import _root_.net.liftweb.http.js.jquery._
import _root_.net.liftweb.http.provider._
import _root_.scala.xml._
import _root_.scala.collection.mutable.{ListBuffer}
import _root_.java.util.{Locale, TimeZone, ResourceBundle, Date}
import _root_.java.io.{InputStream, ByteArrayOutputStream, BufferedReader, StringReader}
import js._
import JE._
import auth._
import _root_.java.util.concurrent.{ConcurrentHashMap => CHash}
import _root_.scala.reflect.Manifest

object LiftRules extends Factory {
  val noticesContainerId = "lift__noticesContainer__"

  type DispatchPF = PartialFunction[Req, () => Box[LiftResponse]];
  type RewritePF = PartialFunction[RewriteRequest, RewriteResponse]
  type SnippetPF = PartialFunction[List[String], NodeSeq => NodeSeq]
  type LiftTagPF = PartialFunction[(String, Elem, MetaData, NodeSeq, String), NodeSeq]
  type URINotFoundPF = PartialFunction[(Req, Box[Failure]), LiftResponse]
  type URLDecoratorPF = PartialFunction[String, String]
  type SnippetDispatchPF = PartialFunction[String, DispatchSnippet]
  type ViewDispatchPF = PartialFunction[List[String], Either[() => Box[NodeSeq], LiftView]]
  type HttpAuthProtectedResourcePF = PartialFunction[ParsePath, Box[Role]]
  type ExceptionHandlerPF = PartialFunction[(Props.RunModes.Value, Req, Throwable), LiftResponse]
  type ResourceBundleFactoryPF = PartialFunction[(String,Locale), ResourceBundle]

  /**
   * A partial function that allows the application to define requests that should be
   * handled by lift rather than the default handler
   */
  type LiftRequestPF = PartialFunction[Req, Boolean]

  /**
   * Holds user functions that willbe executed very early in the request processing. The functions'
   * result will be ignored.
   */
  val early = RulesSeq[(HTTPRequest) => Any]

  /**
   * Holds user functions that are executed before sending the response to client. The functions'
   * result will be ignored.
   */
  val beforeSend = RulesSeq[(BasicResponse, HTTPResponse, List[(String, String)], Box[Req]) => Any]

  /**
   * Defines the resources that are protected by authentication and authorization. If this function
   * is notdefined for the input data, the resource is considered unprotected ergo no authentication
   * is performed. If this function is defined and returns a Full can, it means that this resource
   * is protected by authentication,and authenticated subjed must be assigned to the role returned by
   * this function or to a role that is child-of this role. If this function returns Empty it means that
   * this resource is protected by authentication but no authorization is performed meaning that roles are
   * not verified.
   */
  val httpAuthProtectedResource = RulesSeq[HttpAuthProtectedResourcePF]

  /**
   * The HTTP authentication mechanism that ift will perform. See <i>LiftRules.protectedResource</i>
   */
  var authentication : HttpAuthentication = NoAuthentication

  /**
   * A function that takes the HTTPSession and the contextPath as parameters
   * and returns a LiftSession reference. This can be used in cases subclassing
   * LiftSession is necessary.
   */
  var sessionCreator: (HTTPSession,  String) => LiftSession = {
    case (httpSession, contextPath) => new LiftSession(contextPath, httpSession.sessionId, Full(httpSession))
  }

  var enableContainerSessions = true

  var getLiftSession: (Req) => LiftSession = (req) => _getLiftSession(req)

  /**
   * Returns a LiftSession instance.
   */
  private def _getLiftSession(req: Req): LiftSession = {
    val wp = req.path.wholePath
    val cometSessionId =
    if (wp.length >= 3 && wp.head == LiftRules.cometPath)
    Full(wp(2))
    else
    Empty

    val ret = SessionMaster.getSession(req.request, cometSessionId) match {
      case Full(ret) =>
        ret.fixSessionTime()
        ret

      case _ =>
        val ret = LiftSession(req.request.session, req.request.contextPath)
        ret.fixSessionTime()
        SessionMaster.addSession(ret)
        ret
    }

    ret.breakOutComet()
    ret
  }


  /**
   * The path to handle served resources
   */
  var resourceServerPath = "classpath"

  /**
   * Holds the JS library specific UI artifacts. By efault it uses JQuery's artifacts
   */
  var jsArtifacts: JSArtifacts = JQueryArtifacts

  /**
   * Use this PartialFunction to to automatically add static URL parameters
   * to any URL reference from the markup of Ajax request.
   */
  val urlDecorate = RulesSeq[URLDecoratorPF]

  /**
   * Holds user functions that are executed after the response was sent to client. The functions' result
   * will be ignored.
   */
  val afterSend = RulesSeq[(BasicResponse, HTTPResponse, List[(String, String)], Box[Req]) => Any]

  /**
   * Calculate the Comet Server (by default, the server that
   * the request was made on, but can do the multi-server thing
   * as well)
   */
  var cometServer: () => String = () => S.contextPath

  /**
   * The maximum concurrent requests.  If this number of
   * requests are being serviced for a given session, messages
   * will be sent to all Comet requests to terminate
   */
  var maxConcurrentRequests = 2

  /**
   * A partial function that determines content type based on an incoming
   * Req and Accept header
   */
  var determineContentType: PartialFunction[(Box[Req], Box[String]), String] = {
    case (_, Full(accept)) if this.useXhtmlMimeType && accept.toLowerCase.contains("application/xhtml+xml") =>
      "application/xhtml+xml; charset=utf-8"
    case _ => "text/html; charset=utf-8"
  }

  lazy val liftVersion: String = {
    val cn = """\.""".r.replaceAllIn(LiftRules.getClass.getName, "/")
    val ret: Box[String] =
    for {
      url <- Box !! LiftRules.getClass.getResource("/"+cn+".class")
      val newUrl = new _root_.java.net.URL(url.toExternalForm.split("!")(0)+"!"+"/META-INF/MANIFEST.MF")
      str <- tryo(new String(readWholeStream(newUrl.openConnection.getInputStream), "UTF-8"))
      ma <- """lift_version: (.*)""".r.findFirstMatchIn(str)
    } yield ma.group(1)

    ret openOr "Unknown Lift Version"
  }

  lazy val liftBuildDate: Date = {
    val cn = """\.""".r.replaceAllIn(LiftRules.getClass.getName, "/")
    val ret: Box[Date] =
    for {
      url <- Box !! LiftRules.getClass.getResource("/"+cn+".class")
      val newUrl = new _root_.java.net.URL(url.toExternalForm.split("!")(0)+"!"+"/META-INF/MANIFEST.MF")
      str <- tryo(new String(readWholeStream(newUrl.openConnection.getInputStream), "UTF-8"))
      ma <- """Bnd-LastModified: (.*)""".r.findFirstMatchIn(str)
      asLong <- asLong(ma.group(1))
    } yield new Date(asLong)

    ret openOr new Date(0L)
  }

  /**
   * Hooks to be run when LiftServlet.destroy is called.
   */
  val unloadHooks = RulesSeq[() => Unit]

  /**
   * For each unload hook registered, run them during destroy()
   */
  private[http] def runUnloadHooks() {
    unloadHooks.toList.foreach(_())
  }

  /**
   * The maximum allowed size of a complete mime multi-part POST.  Default
   * 8MB
   */
  var maxMimeSize: Long = 8 * 1024 * 1024

  /**
   * Should pages that are not found be passed along the request processing chain to the
   * next handler outside Lift?
   */
  var passNotFoundToChain = false

  /**
   * The maximum allowed size of a single file in a mime multi-part POST.
   * Default 7MB
   */
  var maxMimeFileSize: Long = 7 * 1024 * 1024

  /**
   * The function referenced here is called if there's a localization lookup failure
   */
  var localizationLookupFailureNotice: Box[(String, Locale) => Unit] = Empty

  /**
   * The default location to send people if SiteMap access control fails. The path is
   * expressed a a List[String]
   */
  var siteMapFailRedirectLocation: List[String] = List()

  private[http] def notFoundOrIgnore(requestState: Req, session: Box[LiftSession]): Box[LiftResponse] = {
    if (passNotFoundToChain) Empty
    else session match {
      case Full(session) => Full(session.checkRedirect(requestState.createNotFound))
      case _ => Full(requestState.createNotFound)
    }
  }

  /**
   * Allows user adding additional Lift tags (the tags must be prefixed by lift namespace such as <lift:xxxx/>).
   * Each LiftTagPF function will be called with the folowing parameters:
   * <pre>
   *  - Element label,
   *  - The Element itselft,
   *  - The attrbutes
   *  - The child nodes
   *  - The page name
   * </pre>
   */
  val liftTagProcessing = RulesSeq[LiftTagPF]

  /**
   * If you don't want lift to send the application/xhtml+xml mime type to those browsers
   * that understand it, then set this to {@code false}
   */
  var useXhtmlMimeType: Boolean = true


  private def _stringToXml(s: String): NodeSeq = Text(s)

  /**
   * A function that defines how a String should be converted to XML
   * for the localization stuff.  By default, Text(s) is returned,
   * but you can change this to attempt to parse the XML in the String and
   * return the NodeSeq.
   */
  var localizeStringToXml: String => NodeSeq = _stringToXml _

  /**
   * The base name of the resource bundle
   */
  var resourceNames: List[String] = List("lift")

  var noticesToJsCmd: () => JsCmd = () => {
    import builtin.snippet._

    val func: (() => List[NodeSeq], String, MetaData) => NodeSeq = (f, title, attr) => f() map (e => <li>{e}</li>) match {
      case x if x.isEmpty => NodeSeq.Empty
      case list => <div>{title}<ul>{list}</ul></div> % attr
    }

    val f = S.noIdMessages _
    val xml = List((MsgsErrorMeta.get, f(S.errors), S.??("msg.error")),
                   (MsgsWarningMeta.get, f(S.warnings), S.??("msg.warning")),
                   (MsgsNoticeMeta.get, f(S.notices), S.??("msg.notice"))) flatMap {
      msg => msg._1 match {
        case Full(meta) => func(msg._2 _, meta.title openOr "", meta.cssClass.map(new UnprefixedAttribute("class", _, Null)) openOr Null)
        case _ => func(msg._2 _, msg._3, Null)
      }
    }

    val groupMessages = 
      if (xml.isEmpty) JsCmds.Noop
      else LiftRules.jsArtifacts.setHtml(LiftRules.noticesContainerId, xml)

    val g = S.idMessages _
    List((MsgErrorMeta.get, g(S.errors)),
         (MsgWarningMeta.get, g(S.warnings)),
         (MsgNoticeMeta.get, g(S.notices))).foldLeft(groupMessages)((car, cdr) => cdr match {
        case (meta, m) => m.foldLeft(car)((left, r) =>
            left & LiftRules.jsArtifacts.setHtml(r._1, <span>{r._2 flatMap(node => node)}</span> %
                                                 (Box(meta.get(r._1)).map(new UnprefixedAttribute("class", _, Null)) openOr Null)))
      })
  }

  /**
   * The base name of the resource bundle of the lift core code
   */
  var liftCoreResourceName = "i18n.lift-core"

  /**
   * Where to send the user if there's no comet session
   */
  var noCometSessionPage = "/"

  /**
   * Put a function that will calculate the request timeout based on the
   * incoming request.
   */
  var calcRequestTimeout: Box[Req => Int] = Empty

  /**
   * If you want the standard (non-AJAX) request timeout to be something other than
   * 10 seconds, put the value here
   */
  var stdRequestTimeout: Box[Int] = Empty

  /**
   * If you want the AJAX request timeout to be something other than 120 seconds, put the value here
   */
  var cometRequestTimeout: Box[Int] = Empty

  /**
   * If a Comet request fails timeout for this period of time. Default value is 10 seconds
   */
  var cometFailureRetryTimeout: Long = 10 seconds

  /**
   * The dispatcher that takes a Snippet and converts it to a
   * DispatchSnippet instance
   */
  val snippetDispatch = RulesSeq[SnippetDispatchPF]
  private def setupSnippetDispatch() {
    import net.liftweb.builtin.snippet._

    snippetDispatch.append(
      Map("CSS" -> CSS, "Msgs" -> Msgs, "Msg" -> Msg,
          "Menu" -> Menu, "css" -> CSS, "msgs" -> Msgs, "msg" -> Msg,
          "menu" -> Menu,
          "a" -> A, "children" -> Children,
          "comet" -> Comet, "form" -> Form, "ignore" -> Ignore, "loc" -> Loc,
          "surround" -> Surround,
          "test_cond" -> TestCond,
          "embed" -> Embed,
          "tail" -> Tail,
          "with-param" -> WithParam,
          "bind-at" -> WithParam,
          "VersionInfo" -> VersionInfo,
          "version_info" -> VersionInfo,
          "SkipDocType" -> SkipDocType,
          "skip_doc_type" -> SkipDocType,
          "xml_group" -> XmlGroup,
          "XmlGroup" -> XmlGroup
      ))
  }
  setupSnippetDispatch()


  /**
   * Change this variable to set view dispatching
   */
  val viewDispatch = RulesSeq[ViewDispatchPF]

  private[http] def snippet(name: String): Box[DispatchSnippet] = NamedPF.applyBox(name, snippetDispatch.toList)

  /**
   * If the request times out (or returns a non-Response) you can
   * intercept the response here and create your own response
   */
  var requestTimedOut: Box[(Req, Any) => Box[LiftResponse]] = Empty

  /**
   * A function that takes the current HTTP request and returns the current
   */
  var timeZoneCalculator: Box[HTTPRequest] => TimeZone = defaultTimeZoneCalculator _

  def defaultTimeZoneCalculator(request: Box[HTTPRequest]): TimeZone = TimeZone.getDefault

  /**
   * How many times do we retry an Ajax command before calling it a failure?
   */
  var ajaxRetryCount: Box[Int] = Empty

  /**
   * The JavaScript to execute at the begining of an
   * Ajax request (for example, showing the spinning working thingy)
   */
  var ajaxStart: Box[() => JsCmd] = Empty

  /**
   * The function that calculates if the response should be rendered in
   * IE6/7 compatibility mode
   */
  var calcIEMode: () => Boolean =
  () => (for (r <- S.request) yield r.isIE6 || r.isIE7 ||
         r.isIE8) openOr true

  /**
   * The JavaScript to execute at the end of an
   * Ajax request (for example, removing the spinning working thingy)
   */
  var ajaxEnd: Box[() => JsCmd] = Empty

  /**
   * The default action to take when the JavaScript action fails
   */
  var ajaxDefaultFailure: Box[() => JsCmd] =
  Full(() => JsCmds.Alert(S.??("ajax.error")))

  /**
   * A function that takes the current HTTP request and returns the current
   */
  var localeCalculator: Box[HTTPRequest] => Locale = defaultLocaleCalculator _

  def defaultLocaleCalculator(request: Box[HTTPRequest]) =
  request.flatMap(_.locale).openOr(Locale.getDefault())

  var resourceBundleFactories = RulesSeq[ResourceBundleFactoryPF]

  /**
   * User for Comet handling to resume a continuation
   */
  def resumeRequest(what: AnyRef, req: HTTPRequest) = req resume what

  /**
   * Execute a continuation. For Jetty the Jetty specific exception will be thrown
   * and the container will manage it.
   */
  def doContinuation(req: HTTPRequest, timeout: Long): Nothing =  req suspend timeout

  /**
   * Check to see if continuations are supported
   */
  def checkContinuations(req: HTTPRequest): Option[Any] = req hasSuspendResumeSupport_?

  private var _sitemap: Box[SiteMap] = Empty

  def setSiteMap(sm: SiteMap) {
    _sitemap = Full(sm)
    for (menu <- sm.menus;
         val loc = menu.loc;
         rewrite <- loc.rewritePF) LiftRules.rewrite.append(rewrite)
  }

  def siteMap: Box[SiteMap] = _sitemap

  private[http] var ending = false

  private[http] var doneBoot = false;

  /**
   * Holds user's DispatchPF functions that will be executed in a stateless context. This means that
   * S object is not availble yet.
   */
  val statelessDispatchTable = RulesSeq[DispatchPF]

  private[http] def dispatchTable(req: HTTPRequest): List[DispatchPF] = {
    req match {
      case null => dispatch.toList
      case _ => SessionMaster.getSession(req, Empty) match {
          case Full(s) => S.initIfUninitted(s) {
              S.highLevelSessionDispatchList.map(_.dispatch) :::
              dispatch.toList
            }
          case _ => dispatch.toList
        }
    }
  }

  private[http] def rewriteTable(req: HTTPRequest): List[RewritePF] = {
    req match {
      case null => rewrite.toList
      case _ => SessionMaster.getSession(req, Empty) match {
          case Full(s) => S.initIfUninitted(s) {
              S.sessionRewriter.map(_.rewrite) ::: LiftRules.rewrite.toList
            }
          case _ => rewrite.toList
        }
    }
  }

  /**
   * Contains the Ajax URI path used by Lift to process Ajax requests.
   */
  var ajaxPath = "ajax_request"

  /**
   * Contains the Comet URI path used by Lift to process Comet requests.
   */
  var cometPath = "comet_request"

  /**
   * Computes the Comet path by adding additional tokens on top of cometPath
   */
  var calcCometPath: String => JsExp = prefix => Str(prefix + "/" + cometPath + "/") +
  JsRaw("Math.floor(Math.random() * 100000000000)") +
  Str(S.session.map(s => "/"+s.uniqueId) openOr "")

  /**
   * The default way of calculating the context path
   */
  def defaultCalcContextPath(request: HTTPRequest): Box[String] = {
    request.header("X-Lift-ContextPath").map {
      case s if s.trim == "/" => ""
      case s => s.trim
    }
  }

  /**
   * If there is an alternative way of calculating the context path
   * (by default inspecting the X-Lift-ContextPath header)
   */
  var calculateContextPath: HTTPRequest => Box[String] =
  defaultCalcContextPath _

  private var _context: HTTPContext = _

  /**
   * Returns the HTTPContext
   */
  def context: HTTPContext = synchronized {_context}

  /**
   * Sets the HTTPContext
   */
  def setContext(in: HTTPContext): Unit =  synchronized {
    if (in ne _context) {
      _context = in
    }
  }

  private var otherPackages: List[String] = Nil

  /**
   * Used by Lift to construct full pacakge names fromthe packages provided to addToPackages function
   */
  def buildPackage(end: String) = synchronized (otherPackages.map(_+"."+end))

  /**
   * Tells Lift where to find Snippets,Views, Comet Actors and Lift ORM Model object
   */
  def addToPackages(what: String) {
    synchronized {otherPackages = what :: otherPackages}
  }

  /**
   * Tells Lift where to find Snippets,Views, Comet Actors and Lift ORM Model object
   */
  def addToPackages(what: Package) {
    synchronized {otherPackages = what.getName :: otherPackages}
  }

  private val defaultFinder = getClass.getResource _
  private def resourceFinder(name: String): _root_.java.net.URL = _context.resource(name)

  /**
   * Obtain the resource URL by name
   */
  var getResource: String => Box[_root_.java.net.URL] = defaultGetResource _

  /**
   * Obtain the resource URL by name
   */
  def defaultGetResource(name: String): Box[_root_.java.net.URL] =
  for {
    rf <- (Box !! resourceFinder(name)) or (Box !! defaultFinder(name))
  } yield rf
  // resourceFinder(name) match {case null => defaultFinder(name) match {case null => Empty; case s => Full(s)} ; case s => Full(s)}

  /**
   * Obtain the resource InputStream by name
   */
  def getResourceAsStream(name: String): Box[InputStream] =
  getResource(name).map(_.openStream)

  /**
   * Obtain the resource as an array of bytes by name
   */
  def loadResource(name: String): Box[Array[Byte]] = getResourceAsStream(name).map{
    stream =>
    val buffer = new Array[Byte](2048)
    val out = new ByteArrayOutputStream
    def reader {
      val len = stream.read(buffer)
      if (len < 0) return
      else if (len > 0) out.write(buffer, 0, len)
      reader
    }
    reader
    stream.close
    out.toByteArray
  }

  /**
   * Obtain the resource as an XML by name
   */
  def loadResourceAsXml(name: String): Box[NodeSeq] = loadResourceAsString(name).flatMap(s => PCDataXmlParser(s))

  /**
   * Obtain the resource as a String by name
   */
  def loadResourceAsString(name: String): Box[String] = loadResource(name).map(s => new String(s, "UTF-8"))

  /**
   * Looks up a resource by name and returns an Empty Box if the resource was not found.
   */
  def finder(name: String): Box[InputStream] =
  (for {
      ctx <- Box !! LiftRules.context
      res <- Box !! ctx.resourceAsStream(name)
    } yield res) or getResourceAsStream(name)

  /**
   * Get the partial function that defines if a request should be handled by
   * the application (rather than the default container handler)
   */
  val liftRequest = RulesSeq[LiftRequestPF]

  /**
   * Holds the user's DispatchPF functions that will be executed in stateful context
   */
  val dispatch = RulesSeq[DispatchPF]

  /**
   * Holds the user's rewrite functions that can alter the URI parts and query parameters
   */
  val rewrite = RulesSeq[RewritePF]

  /**
   * Holds the user's snippet functions that will be executed by lift given a certain path.
   */
  val snippets = RulesSeq[SnippetPF]

  private val _cometLogger: FatLazy[LiftLogger] = FatLazy({
      val ret = LogBoot.loggerByName("comet_trace")
      ret.level = LiftLogLevels.Off
      ret
    })

  /**
   * Holds the CometLogger that will be used to log comet activity
   */
  def cometLogger: LiftLogger = _cometLogger.get

  /**
   * Holds the CometLogger that will be used to log comet activity
   */
  def cometLogger_=(newLogger: LiftLogger): Unit = _cometLogger.set(newLogger)

  /**
   * Takes a Node, headers, cookies, and a session and turns it into an XhtmlResponse.
   */
  private def cvt(ns: Node, headers: List[(String, String)], cookies: List[HTTPCookie], session: Req) =
  convertResponse({val ret = XhtmlResponse(ns,
                                           ResponseInfo.docType(session),
                                           headers, cookies, 200,
                                           S.ieMode)
                   ret._includeXmlVersion = !S.skipDocType
                   ret
    }, headers, cookies, session)

  var defaultHeaders: PartialFunction[(NodeSeq, Req), List[(String, String)]] = {
    case _ => List("Expires" -> Helpers.nowAsInternetDate,
                   "Cache-Control" ->
                   "no-cache; private; no-store; must-revalidate; max-stale=0; post-check=0; pre-check=0; max-age=0",
                   "Pragma" -> "no-cache" /*,
      "Keep-Alive" -> "timeout=3, max=993" */)
  }

  /**
   * Runs responseTransformers
   */
  def performTransform(in: LiftResponse): LiftResponse = responseTransformers.toList.foldLeft(in) {
    case (in, pf: PartialFunction[_, _]) =>
      if (pf.isDefinedAt(in)) pf(in) else in
    case (in, f) => f(in)
  }

  /**
   * Holds the user's transformer functions allowing the user to modify a LiftResponse before sending it to client.
   */
  val responseTransformers = RulesSeq[LiftResponse => LiftResponse]

  /**
   * Calculate the context path for a given session if it should be something different than
   * the normal context path
   */
  val calcContextPath: LiftSession => Box[String] = _ => Empty

  /**
   * convertResponse is a PartialFunction that reduces a given Tuple4 into a
   * LiftResponse that can then be sent to the browser.
   */
  var convertResponse: PartialFunction[(Any, List[(String, String)], List[HTTPCookie], Req), LiftResponse] = {
    case (r: LiftResponse, _, _, _) => r
    case (ns: Group, headers, cookies, session) => cvt(ns, headers, cookies, session)
    case (ns: Node, headers, cookies, session) => cvt(ns, headers, cookies, session)
    case (ns: NodeSeq, headers, cookies, session) => cvt(Group(ns), headers, cookies, session)
    case (SafeNodeSeq(n), headers, cookies, session) => cvt(Group(n), headers, cookies, session)

    case (Full(o), headers, cookies, session) => convertResponse( (o, headers, cookies, session) )

    case (Some(o), headers, cookies, session) => convertResponse( (o, headers, cookies, session) )
    case (bad, _, _, session) => session.createNotFound
  }

  /**
   * Set a snippet failure handler here.  The class and method for the snippet are passed in
   */
  val snippetFailedFunc = RulesSeq[SnippetFailure => Unit].prepend(logSnippetFailure _)

  private def logSnippetFailure(sf: SnippetFailure) = Log.warn("Snippet Failure: "+sf)

  /**
   * Set to false if you do not want Ajax/Comet requests that are not associated with a session
   * to cause a page reload
   */
  var redirectAjaxOnSessionLoss = true

  /**
   * Holds the falure information when a snippet can not be executed.
   */
  case class SnippetFailure(page: String, typeName: Box[String], failure: SnippetFailures.Value)

  object SnippetFailures extends Enumeration {
    val NoTypeDefined = Value(1, "No Type Defined")
    val ClassNotFound = Value(2, "Class Not Found")
    val StatefulDispatchNotMatched = Value(3, "Stateful Snippet: Dispatch Not Matched")
    val MethodNotFound = Value(4, "Method Not Found")
    val NoNameSpecified = Value(5, "No Snippet Name Specified")
    val InstantiationException = Value(6, "Exception During Snippet Instantiation")
    val DispatchSnippetNotMatched = Value(7, "Dispatch Snippet: Dispatch Not Matched")
  }

  /**
   * The sequence of partial functions (pattern matching) for handling converting an exception to something to
   * be sent to the browser depending on the current RunMode (development, etc.)
   *
   * By default it returns an XhtmlResponse containing a predefined markup. You can overwrite this by calling
   * LiftRules.exceptionHandler.prepend(...). If you are calling append then your code will not be calle since
   * a default implementation is already appended.
   *
   */
  var exceptionHandler = RulesSeq[ExceptionHandlerPF].append {
    case (Props.RunModes.Development, r, e) =>
      XhtmlResponse((<html><body>Exception occured while processing {r.uri}
              <pre>{
                  showException(e)
                }</pre></body></html>),ResponseInfo.docType(r), List("Content-Type" -> "text/html; charset=utf-8"), Nil, 500, S.ieMode)

    case (_, r, e) =>
      Log.error("Exception being returned to browser when processing "+r, e)
      XhtmlResponse((<html><body>Something unexpected happened while serving the page at {r.uri}
                           </body></html>),ResponseInfo.docType(r), List("Content-Type" -> "text/html; charset=utf-8"), Nil, 500, S.ieMode)
  }

  /**
   * The list of partial function for defining the behavior of what happens when
   * URI is invalid and you're not using a site map
   *
   */
  val uriNotFound = RulesSeq[URINotFoundPF].prepend(NamedPF("default") {
      case (r, _) => Req.defaultCreateNotFound(r)
    })

  /**
   * If you use the form attribute in a snippet invocation, what attributes should
   * be copied from the snippet invocation tag to the form tag.  The
   * default list is "class", "id", "target", "style", "onsubmit"
   */
   val formAttrs: FactoryMaker[List[String]] = new FactoryMaker(() => List("class", "id", "target", "style", "onsubmit")) {}

  /**
   * A utility method to convert an exception to a string of stack traces
   * @param le the exception
   *
   * @return the stack trace
   */
  private def showException(le: Throwable): String = {
    val ret = "Message: "+le.toString+"\n\t"+
    le.getStackTrace.map(_.toString).mkString("\n\t") + "\n"

    val also = le.getCause match {
      case null => ""
      case sub: Throwable => "\nCaught and thrown by:\n"+ showException(sub)
    }

    ret + also
  }

  /**
   * Modifies the root relative paths from the css url-s
   *
   * @param path - the path of the css resource
   * @prefix - the prefix to be added on the root relative paths. If this is Empty
   * 	       the prefix will be the application context path.
   */
  def fixCSS(path: List[String], prefix: Box[String]) {

    val liftReq: LiftRules.LiftRequestPF = new LiftRules.LiftRequestPF {
      def functionName = "Default CSS Fixer"

      def isDefinedAt(r: Req): Boolean = {
        r.path.partPath == path
      }
      def apply(r: Req): Boolean = {
        r.path.partPath == path
      }
    }

    val cssFixer: LiftRules.DispatchPF = new LiftRules.DispatchPF {
      def functionName = "default css fixer"
      def isDefinedAt(r: Req): Boolean = {
        r.path.partPath == path
      }
      def apply(r: Req): () => Box[LiftResponse] = {
        val cssPath = path.mkString("/", "/", ".css")
        val css = LiftRules.loadResourceAsString(cssPath);

        () => {
          css.map(str => CSSHelpers.fixCSS(new BufferedReader(
                new StringReader(str)), prefix openOr (S.contextPath)) match {
              case (Full(c), _) => CSSResponse(c)
              case (_, input) => {
                  Log.warn("Fixing " + cssPath + " failed");
                  CSSResponse(input)
                }
            })
        }
      }
    }
    LiftRules.dispatch.prepend(cssFixer)
    LiftRules.liftRequest.append(liftReq)
  }

  /**
   * Holds user function hooks when the request is about to be processed
   */
  val onBeginServicing = RulesSeq[Req => Unit]

  val preAccessControlResponse_!! = new RulesSeq[Req => Box[LiftResponse]] with FirstBox[Req, LiftResponse]

  val earlyResponse = new RulesSeq[Req => Box[LiftResponse]] with FirstBox[Req, LiftResponse]

  /**
   * Holds user function hooks when the request was processed
   */
  val onEndServicing = RulesSeq[(Req, Box[LiftResponse]) => Unit]

  /**
   * Tells Lift if the Comet JavaScript shoukd be included. By default it is set to true.
   */
  var autoIncludeComet: LiftSession => Boolean = session => true

  /**
   * Tells Lift if the Ajax JavaScript shoukd be included. By default it is set to true.
   */
  var autoIncludeAjax: LiftSession => Boolean = session => true

  /**
   * Define the XHTML validator
   */
  var xhtmlValidator: Box[XHtmlValidator] = Empty // Full(TransitionalXHTML1_0Validator)

  /**
   * Returns the JavaScript that manages Ajax requests.
   */
  var renderAjaxScript: LiftSession => JsCmd = session => ScriptRenderer.ajaxScript

  var ajaxPostTimeout = 5000

  var cometGetTimeout = 140000

  var supplimentalHeaders: HTTPResponse => Unit = s => s.addHeaders(List(HTTPParam("X-Lift-Version", liftVersion)))

  var calcIE6ForResponse: () => Boolean = () => S.request.map(_.isIE6) openOr false

  var flipDocTypeForIE6 = true

  /**
   * By default lift uses a garbage-collection mechanism of removing unused bound functions from LiftSesssion.
   * Setting this to false will disable this mechanims and there will be no Ajax polling requests attempted.
   */
  var enableLiftGC = true;

  /**
   * If Lift garbage collection is enabled, functions that are not seen in the page for this period of time
   * (given in milliseonds) will be discarded, hence eligibe for garbage collection.
   * The default value is 10 minutes.
   */
  var unusedFunctionsLifeTime: Long = 10 minutes

  /**
   * The polling interval for background Ajax requests to prevent functions of being garbage collected.
   * Default value is set to 75 seconds.
   */
  var liftGCPollingInterval: Long = 75 seconds

  /**
   * Put a test for being logged in into this function
   */
  var loggedInTest: Box[() => Boolean] = Empty

  /**
   * The polling interval for background Ajax requests to keep functions to not be garbage collected.
   * This will be applied if the Ajax request will fail. Default value is set to 15 seconds.
   */
  var liftGCFailureRetryTimeout: Long = 15 seconds

  /**
   * Returns the JavaScript that manages Comet requests.
   */
  var renderCometScript: LiftSession => JsCmd = session => ScriptRenderer.cometScript

  /**
   * Renders that JavaScript that holds Comet identification information
   */
  var renderCometPageContents: (LiftSession, Seq[CometVersionPair]) => JsCmd =
  (session, vp) => JsCmds.Run(
    "var lift_toWatch = "+vp.map(p => p.guid.encJs+": "+p.version).mkString("{", " , ", "}")+";"
  )

  /**
   * Hods the last update time of the Ajax request. Based on this server mayreturn HTTP 304 status
   * indicating the client to used the cached information.
   */
  var ajaxScriptUpdateTime: LiftSession => Long = session => {
    object when extends SessionVar[Long](millis)
    when.is
  }

  /**
   * The global multipart progress listener:
   *     pBytesRead - The total number of bytes, which have been read so far.
   *    pContentLength - The total number of bytes, which are being read. May be -1, if this number is unknown.
   *    pItems - The number of the field, which is currently being read. (0 = no item so far, 1 = first item is being read, ...)
   */
  var progressListener: (Long, Long, Int) => Unit = (_, _, _) => ()

  /**
   * The function that converts a fieldName, contentType, fileName and an InputStream into
   * a FileParamHolder.  By default, create an in-memory instance.  Use OnDiskFileParamHolder
   * to create an on-disk version
   */
  var handleMimeFile: (String, String, String, InputStream) => FileParamHolder =
  (fieldName, contentType, fileName, inputStream) =>
  new InMemFileParamHolder(fieldName, contentType, fileName, Helpers.readWholeStream(inputStream))

  /**
   * Hods the last update time of the Comet request. Based on this server mayreturn HTTP 304 status
   * indicating the client to used the cached information.
   */
  var cometScriptUpdateTime: LiftSession => Long = session => {
    object when extends SessionVar[Long](millis)
    when.is
  }

  /**
   * The name of the Ajax script that manages Ajax rewuests.
   */
  var ajaxScriptName: () => String = () => "liftAjax.js"

  /**
   * The name of the Comet script that manages Comet rewuests.
   */
  var cometScriptName: () => String = () => "cometAjax.js"

  /**
   * Returns the Comet script as a JavaScript response
   */
  var serveCometScript: (LiftSession, Req) => Box[LiftResponse] =
  (liftSession, requestState) => {
    val modTime = cometScriptUpdateTime(liftSession)

    requestState.testFor304(modTime) or
    Full(JavaScriptResponse(renderCometScript(liftSession),
                            List("Last-Modified" -> toInternetDate(modTime),
                                 "Expires" -> toInternetDate(modTime + 10.minutes)),
                            Nil, 200))
  }

  /**
   * Returns the Ajax script as a JavaScript response
   */
  var serveAjaxScript: (LiftSession, Req) => Box[LiftResponse] =
  (liftSession, requestState) => {
    val modTime = ajaxScriptUpdateTime(liftSession)

    requestState.testFor304(modTime) or
    Full(JavaScriptResponse(renderAjaxScript(liftSession),
                            List("Last-Modified" -> toInternetDate(modTime),
                                 "Expires" -> toInternetDate(modTime + 10.minutes)),
                            Nil, 200))
  }

  var templateCache: Box[TemplateCache[(Locale, List[String]), NodeSeq]] = Empty

  /**
   * A function to format a Date... can be replaced by a function that is user-specific
   */
  var formatDate: Date => String = date => date match {case null => LiftRules.formatDate(new Date(0L)) case s => toInternetDate(s)}

  /**
   * A function that parses a String into a Date... can be replaced by something that's user-specific
   */
  var parseDate: String => Box[Date] = str => str match {
    case null => Empty
    case s => Helpers.toDate(s)
  }
}

case object BreakOut

abstract class Bootable {
  def boot() : Unit;
}

/**
 * Factory object for RulesSeq instances
 */
object RulesSeq {
  def apply[T]: RulesSeq[T] = new RulesSeq[T]{}
}

/**
 * Generic container used mainly for adding functions
 *
 */
trait RulesSeq[T] {
  private var rules: List[T] = Nil

  private def safe_?(f : => Any) {
    LiftRules.doneBoot match {
      case false => f
      case _ => throw new IllegalStateException("Cannot modify after boot.");
    }
  }

  def toList = rules

  def prepend(r: T): RulesSeq[T] = {
    safe_? {
      rules = r :: rules
    }
    this
  }

  def append(r: T): RulesSeq[T] = {
    safe_? {
      rules = rules ::: List(r)
    }
    this
  }
}

trait FirstBox[F, T] {
  self: RulesSeq[F => Box[T]] =>

  def firstFull(param: F): Box[T] = {
    def finder(in: List[F => Box[T]]): Box[T] = in match {
      case Nil => Empty
      case x :: xs => x(param) match {
          case Full(r) => Full(r)
          case _ => finder(xs)
        }
    }

    finder(toList)
  }
}

private[http] case object DefaultBootstrap extends Bootable {
  def boot() : Unit = {
    val f = createInvoker("boot", Class.forName("bootstrap.liftweb.Boot").newInstance.asInstanceOf[AnyRef])
    f.map{f => f()}
  }
}

/**
 * Holds the Comet identification information
 */
trait CometVersionPair {
  def guid: String
  def version: Long
}

case class CVP(guid: String, version: Long) extends CometVersionPair

case class XHTMLValidationError(msg: String, line: Int, col: Int)

trait XHtmlValidator extends Function1[Node, List[XHTMLValidationError]]

object StrictXHTML1_0Validator extends GenericValidtor {
  val ngurl = "http://www.w3.org/2002/08/xhtml/xhtml1-strict.xsd"
}

abstract class GenericValidtor extends XHtmlValidator {
  import javax.xml.validation._
  import javax.xml._
  import XMLConstants._
  import java.net.URL
  import javax.xml.transform.dom._
  import javax.xml.transform.stream._
  import java.io.ByteArrayInputStream

  private lazy val sf = SchemaFactory.newInstance(W3C_XML_SCHEMA_NS_URI)
  protected def ngurl: String

  private lazy val schema = tryo(sf.newSchema(new URL(ngurl)))

  def apply(in: Node): List[XHTMLValidationError] = {
    (for {
        sc <- schema
        v <- tryo(sc.newValidator)
        source = new StreamSource(new ByteArrayInputStream(in.toString.getBytes("UTF-8")))
      } yield try {
        v.validate(source)
        Nil
      } catch {
        case e: org.xml.sax.SAXParseException =>
          List(XHTMLValidationError(e.getMessage, e.getLineNumber, e.getColumnNumber))
      }) match {
      case Full(x) => x
      case Failure(msg, _, _) =>
        Log.info("XHTML Validation Failure: "+msg)
        Nil
      case _ => Nil
    }
  }
}


object TransitionalXHTML1_0Validator extends GenericValidtor {
  val ngurl = "http://www.w3.org/2002/08/xhtml/xhtml1-transitional.xsd"
}
