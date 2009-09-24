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

import _root_.scala.actors.Actor
import _root_.scala.actors.Actor._
import _root_.scala.collection.mutable.{HashMap, ArrayBuffer, ListBuffer}
import _root_.scala.xml.{NodeSeq, Unparsed, Text}
import _root_.net.liftweb.util._
import Box._
import _root_.net.liftweb.http.js.{JsCmd, AjaxInfo}
import _root_.net.liftweb.util.Helpers._
import _root_.net.liftweb.builtin.snippet._
import _root_.java.lang.reflect.{Method, Modifier, InvocationTargetException}
import _root_.scala.xml.{Node, NodeSeq, Elem, MetaData, Null, UnprefixedAttribute, PrefixedAttribute, XML, Comment, Group}
import _root_.java.io.InputStream
import _root_.java.util.concurrent.TimeUnit
import _root_.java.util.Locale
import js._
import scala.reflect.Manifest
import provider._

object LiftSession {

  /**
   * Returns a reference to a LiftSession dictated by LiftRules#sessionCreator function.
   */
  def apply(session: HTTPSession, contextPath: String) =
  LiftRules.sessionCreator(session, contextPath)

  /**
   * Holds user's functions that will be called when the session is activated
   */
  var onSessionActivate: List[LiftSession => Unit] = Nil

  /**
   * Holds user's functions that will be called when the session is passivated
   */
  var onSessionPassivate: List[LiftSession => Unit] = Nil

  /**
   * Holds user's functions that will be called when the session is setup
   */
  var onSetupSession: List[LiftSession => Unit] = Nil

  /**
   * Holds user's functions that will be called when the session is about to be terminated
   */
  var onAboutToShutdownSession: List[LiftSession => Unit] = Nil

  /**
   * Holds user's functions that will be called when the session is terminated
   */
  var onShutdownSession: List[LiftSession => Unit] = Nil

  /**
   * Holds user's functions that will be called when a stateful request is about to be processed
   */
  var onBeginServicing: List[(LiftSession, Req) => Unit] = Nil

  /**
   * Holds user's functions that will be called when a stateful request has been processed
   */
  var onEndServicing: List[(LiftSession, Req, Box[LiftResponse]) => Unit] = Nil
}


private[http] case class AddSession(session: LiftSession)
private[http] case class RemoveSession(sessionId: String)
case class SessionWatcherInfo(sessions: Map[String, LiftSession])

/**
 * Manages LiftSessions because the servlet container is less than optimal at
 * timing sessions out.
 */
object SessionMaster extends Actor {

  private var sessions: Map[String, LiftSession] = Map.empty
  private object CheckAndPurge

  def getSession(id: String, otherId: Box[String]): Box[LiftSession] = synchronized {
    otherId.flatMap(sessions.get) or Box(sessions.get(id))
  }

  /**
   * Put an Actor in this list and the Actor will receive a message
   * every 10 seconds with the current list of sessions:
   * SessionWatcherInfo
   */
  var sessionWatchers: List[Actor] = Nil

  /**
   * Returns a LiftSession or Empty if not found
   */
  def getSession(httpSession: => HTTPSession, otherId: Box[String]): Box[LiftSession] =
  synchronized {
    otherId.flatMap(sessions.get) or Box(sessions.get(httpSession.sessionId))
  }

  /**
   * Returns a LiftSession or Empty if not found
   */
  def getSession(req: HTTPRequest, otherId: Box[String]): Box[LiftSession] =
  synchronized {
    otherId.flatMap(sessions.get) or Box(sessions.get(req.session.sessionId))
  }

  /**
   * Adds a new session to SessionMaster
   */
  def addSession(liftSession: LiftSession) {
    synchronized {
      sessions = sessions + (liftSession.uniqueId -> liftSession)
    }
    liftSession.startSession()
    liftSession.httpSession.foreach(_.link(liftSession))
  }

  def act = {
    doPing()
    link(ActorWatcher)
    loop {
      react(reaction)
    }
  }

  private val reaction: PartialFunction[Any, Unit] = {
    case RemoveSession(sessionId) =>
      val ses = synchronized(sessions)
      ses.get(sessionId).foreach{s =>
        try {
          s.doShutDown
          try {
            s.httpSession.foreach(_.unlink(s))
          } catch {
            case e => // ignore... sometimes you can't do this and it's okay
          }
        } catch {
          case e => Log.error("Failure in remove session", e)

        } finally {
          synchronized{ sessions = sessions - sessionId }
        }
      }

    case CheckAndPurge =>
      val now = millis
      val ses = synchronized{sessions}
      for ((id, session) <- ses.elements) {
        session.doCometActorCleanup()
        if (now - session.lastServiceTime > session.inactivityLength || session.markedForTermination) {
          Log.info(" Session "+id+" expired")
          this.sendMsg(RemoveSession(id))
        } else {
          session.cleanupUnseenFuncs()
        }
      }
      if (!Props.inGAE) {
        sessionWatchers.foreach(_ ! SessionWatcherInfo(ses))
        doPing()
      }
  }

  // Don't start the actor is we're running in the Google App Engine
  if (!Props.inGAE) {
    this.start
  }

  private[http] def sendMsg(in: Any): Unit =
  if (!Props.inGAE) this ! in
  else {
    this.synchronized{
      tryo {
        if (reaction.isDefinedAt(in)) reaction.apply(in)
      }
    }
  }

  private def doPing() {
    if (!Props.inGAE) {
      try {
        ActorPing schedule(this, CheckAndPurge, 10 seconds)
      } catch {
        case e => Log.error("Couldn't start SessionMaster ping", e)
      }
    }
  }
}


object PageName extends RequestVar[String]("")

/**
 * Information about the page garbage collection
 */
object RenderVersion {
  private object ver extends RequestVar({
      val ret =  Helpers.nextFuncName
      S.addFunctionMap(ret, S.SFuncHolder(ignore => {}))
      ret
    })
  def get: String = ver.is
  def set(value: String) {
    ver(value)
  }
}

/**
 * The LiftSession class containg the session state information
 */
@serializable
class LiftSession(val _contextPath: String, val uniqueId: String,
                  val httpSession: Box[HTTPSession]) {
  import TemplateFinder._

  type AnyActor = {def !(in: Any): Unit}

  @volatile
  private[http] var markedForTermination = false

  @volatile
  private var _running_? = false

  /**
   *  ****IMPORTANT**** when you access messageCallback, it *MUST*
   * be in a block that's synchronized on the owner LiftSession
   */
  private var messageCallback: HashMap[String, S.AFuncHolder] = new HashMap

  private[http] var notices: Seq[(NoticeType.Value, NodeSeq, Box[String])] = Nil

  private var asyncComponents = new HashMap[(Box[String], Box[String]), CometActor]()

  private var asyncById = new HashMap[String, CometActor]()

  private var myVariables: Map[String, Any] = Map.empty

  private var onSessionEnd: List[LiftSession => Unit] = Nil

  private val sessionVarSync = new Object

  @volatile
  private[http] var lastServiceTime = millis

  @volatile
  private[http] var inactivityLength: Long = 30 minutes

  private [http] var highLevelSessionDispatcher = new HashMap[String, LiftRules.DispatchPF]()
  private [http] var sessionRewriter = new HashMap[String, LiftRules.RewritePF]()

  private[http] def startSession(): Unit = {
    _running_? = true
    for (sess <- httpSession) {
      inactivityLength = sess.maxInactiveInterval * 1000L
    }

    lastServiceTime = millis
    LiftSession.onSetupSession.foreach(_(this))
  }

  def running_? = _running_?

  private var cometList: List[AnyActor] = Nil

  private[http] def breakOutComet(): Unit = {
    val cl = synchronized {cometList}
    cl.foreach(_ ! BreakOut)
  }

  private[http] def enterComet(what: AnyActor): Unit = synchronized {
    cometList = what :: cometList
  }

  private[http] def exitComet(what: AnyActor): Unit = synchronized {
    cometList = cometList.filterNot(_ eq what)
  }

  private case class RunnerHolder(name: String, func: S.AFuncHolder, owner: Box[String])

  object ieMode extends SessionVar[Boolean](LiftRules.calcIEMode())

  def terminateHint {
    if (_running_?) {
      markedForTermination = true;
    }
  }
  /**
   * Executes the user's functions based on the query parameters
   */
  def runParams(state: Req): List[Any] = {

    val toRun = {
      // get all the commands, sorted by owner,
      (state.uploadedFiles.map(_.name) ::: state.paramNames).
      flatMap{n => synchronized{messageCallback.get(n)}.map(mcb => RunnerHolder(n, mcb, mcb.owner))}.
      sortWith {
        case ( RunnerHolder(_, _, Full(a)), RunnerHolder(_, _, Full(b))) if a < b => true
        case (RunnerHolder(_, _, Full(a)), RunnerHolder(_, _, Full(b))) if a > b => false
        case (RunnerHolder(an, _, Full(a)), RunnerHolder(bn, _, Full(b))) if a == b => an < bn
        case (RunnerHolder(_,_, Full(_)), _) => false
        case (_, RunnerHolder(_, _, Full(_))) => true
        case (RunnerHolder(a, _, _), RunnerHolder(b, _, _)) => a < b
      }
    }

    def buildFunc(i: RunnerHolder): () => Any = i.func match {
      case bfh: S.BinFuncHolder =>
        () => state.uploadedFiles.filter(_.name == i.name).map(v => bfh(v))
      case normal =>
        () => normal(state.params.getOrElse(i.name,
                                            state.uploadedFiles.filter(_.name == i.name).map(_.fileName)))
    }

    val ret = toRun.map(_.owner).removeDuplicates.flatMap{w =>
      val f = toRun.filter(_.owner == w)
      w match {
        // if it's going to a CometActor, batch up the commands
        case Full(id) if asyncById.contains(id) =>
          asyncById.get(id).toList.
          flatMap(a => a !? (5000, ActionMessageSet(f.map(i => buildFunc(i)), state)) match {
              case Some(li: List[_]) => li
              case li: List[_] => li
              case other => Nil
            })
        case _ => f.map(i => buildFunc(i).apply())
      }
    }

    ret
  }

  /**
   * Updates the internal functions mapping
   */
  def updateFunctionMap(funcs: Map[String, S.AFuncHolder], uniqueId: String, when: Long): Unit = synchronized {
    funcs.foreach{case (name, func) => messageCallback(name) = func.duplicate(uniqueId)}
  }

  /**
   * Set your session-specific progress listener for mime uploads
   *     pBytesRead - The total number of bytes, which have been read so far.
   *    pContentLength - The total number of bytes, which are being read. May be -1, if this number is unknown.
   *    pItems - The number of the field, which is currently being read. (0 = no item so far, 1 = first item is being read, ...)
   */
  var progressListener: Box[(Long, Long, Int) => Unit] = Empty

  /**
   * Called just before the session exits.  If there's clean-up work, override this method
   */
  private[http] def cleanUpSession() {
    messageCallback = HashMap.empty
    notices = Nil
    asyncComponents.clear
    asyncById = HashMap.empty
    myVariables = Map.empty
    onSessionEnd = Nil
    highLevelSessionDispatcher = HashMap.empty
    sessionRewriter = HashMap.empty
  }

  private [http] def fixSessionTime(): Unit = synchronized {
    for (httpSession <- this.httpSession) {
      lastServiceTime = millis // DO NOT REMOVE THIS LINE!!!!!
      val diff = lastServiceTime - httpSession.lastAccessedTime
      val maxInactive = httpSession.maxInactiveInterval.toInt
      val togo: Int = maxInactive - (diff / 1000L).toInt
      // if we're within 2 minutes of session timeout and
      // the Servlet session doesn't seem to have been updated,
      // extends the lifespan of the HttpSession
      if (diff > 1000L && togo < 120) {
        httpSession.setMaxInactiveInterval(maxInactive + 120)
      }
    }
  }

  private[http] def doCometActorCleanup(): Unit = {
    val acl = synchronized {this.asyncComponents.valuesIterator.toList}
    acl.foreach(_ ! ShutdownIfPastLifespan)
  }

  /**
   * Adds a cleanup function that will be executed when session is terminated
   */
  def addSessionCleanup(f: LiftSession => Unit): Unit = synchronized {
    onSessionEnd = f :: onSessionEnd
  }

  private[http] def doShutDown() {
    try {
      if (running_?) this.shutDown()
    } finally {
      if (!Props.inGAE)
      Actor.clearSelf
    }
  }

  private[http] def cleanupUnseenFuncs(): Unit = synchronized {
    if (LiftRules.enableLiftGC) {
      val now = millis
      messageCallback.keysIterator.toList.foreach{k =>
        val f = messageCallback(k)
        if (!f.sessionLife && f.owner.isDefined && (now - f.lastSeen) > LiftRules.unusedFunctionsLifeTime) {
          messageCallback -= k
        }
      }
    }
  }

  /**
   * Return the number if updated functions
   */
  private[http] def updateFuncByOwner(ownerName: String, time: Long): Int = synchronized {
    (0 /: messageCallback)((l, v) => l + (v._2.owner match {
          case Full(owner) if (owner == ownerName) =>
            v._2.lastSeen = time
            1
          case Empty => v._2.lastSeen = time
            1
          case _ => 0
        }))
  }

  private def shutDown() = {
    var done: List[() => Unit] = Nil

    S.initIfUninitted(this) {
      onSessionEnd.foreach(_(this))
      synchronized {
        LiftSession.onAboutToShutdownSession.foreach(_(this))

        // Log.debug("Shutting down session")
        _running_? = false

        SessionMaster.sendMsg(RemoveSession(this.uniqueId))

        asyncComponents.foreach{case (_, comp) => done ::= (() => tryo(comp ! ShutDown))}
        cleanUpSession()
        LiftSession.onShutdownSession.foreach(f => done ::= (() => f(this)))
      }
    }

    done.foreach(_.apply())

  }

  /**
   * Find the template assocaited with the Loc
   */
  private[http] def locTemplate: Box[NodeSeq] =
  for (req <- S.request;
       loc <- req.location;
       template <- loc.template) yield template

  def contextPath = (LiftRules.calcContextPath(this) or S.curRequestContextPath) openOr _contextPath

  /**
   * Manages the merge phase of the rendering pipeline
   */

  private def merge(xhtml: NodeSeq, req: Req): Node = {
  val hasHtmlHeadAndBody: Boolean = xhtml.find {
    case e: Elem if e.label == "html" =>
      e.child.find {
        case e: Elem if e.label == "head" => true
        case _ => false
      }.isDefined &&
       e.child.find {
        case e: Elem if e.label == "body" => true
        case _ => false
      }.isDefined
    case _ => false
  }.isDefined

  if (!hasHtmlHeadAndBody) {
    req.fixHtml(xhtml).find {
      case e: Elem => true
      case _ => false
    } getOrElse Text("")
  } else {
 var htmlTag = <html xmlns="http://www.w3.org/1999/xhtml" xmlns:lift='http://liftweb.net'/>
    var headTag = <head/>
    var bodyTag = <body/>
    val headChildren = new ListBuffer[Node]
    val bodyChildren = new ListBuffer[Node]
    val addlHead = new ListBuffer[Node]
    val addlTail = new ListBuffer[Node]
    val cometTimes = new ListBuffer[CometVersionPair]
    val rewrite = URLRewriter.rewriteFunc
    val fixHref = Req.fixHref

    val contextPath: String = this.contextPath

    def fixAttrs(original: MetaData, toFix : String, attrs : MetaData, fixURL: Boolean) : MetaData = attrs match {
      case Null => Null
      case p: PrefixedAttribute if p.key == "when" && p.pre == "lift" =>
        val when = p.value.text
        original.find(a => !a.isPrefixed && a.key == "id").map {
          id =>
          cometTimes += CVP(id.value.text, when.toLong)
        }
        fixAttrs(original, toFix, p.next, fixURL)
      case u: UnprefixedAttribute if u.key == toFix =>
        new UnprefixedAttribute(toFix, fixHref(contextPath, attrs.value, fixURL, rewrite),fixAttrs(original, toFix, attrs.next, fixURL))
      case _ => attrs.copy(fixAttrs(original, toFix, attrs.next, fixURL))

    }

    def _fixHtml(in: NodeSeq, _inHtml: Boolean, _inHead: Boolean, _justHead: Boolean, _inBody: Boolean, _justBody: Boolean, _bodyHead: Boolean, _bodyTail: Boolean): NodeSeq = {
      in.map{
        v =>
        var inHtml = _inHtml
        var inHead = _inHead
        var justHead = false
        var justBody = false
        var inBody = _inBody
        var bodyHead = false
        var bodyTail = false

        v match {
          case e: Elem if e.label == "html" && !inHtml => htmlTag = e; inHtml = true
          case e: Elem if e.label == "head" && inHtml && !inBody => headTag = e; inHead = true; justHead = true
          case e: Elem if e.label == "head" && inHtml && inBody => bodyHead = true 
          case e: Elem if e.label == "tail" && inHtml && inBody => bodyTail = true
          case e: Elem if e.label == "body" && inHtml => bodyTag = e; inBody = true; justBody = true

          case _ =>
        }

        val ret: Node = v match {
          case Group(nodes) => Group(_fixHtml( nodes, inHtml, inHead, justHead, inBody, justBody, bodyHead, bodyTail))
          case e: Elem if e.label == "form" => Elem(v.prefix, v.label, fixAttrs(v.attributes, "action", v.attributes, true), v.scope, _fixHtml(v.child, inHtml, inHead, justHead, inBody, justBody, bodyHead, bodyTail) : _* )
          case e: Elem if e.label == "script" => Elem(v.prefix, v.label, fixAttrs(v.attributes, "src", v.attributes, false), v.scope, _fixHtml(v.child, inHtml, inHead, justHead, inBody, justBody, bodyHead, bodyTail) : _* )
          case e: Elem if e.label == "a" => Elem(v.prefix, v.label, fixAttrs(v.attributes, "href", v.attributes, true), v.scope, _fixHtml( v.child, inHtml, inHead, justHead, inBody, justBody, bodyHead, bodyTail) : _* )
          case e: Elem if e.label == "link" => Elem(v.prefix, v.label, fixAttrs(v.attributes, "href", v.attributes, false), v.scope, _fixHtml( v.child, inHtml, inHead, justHead, inBody, justBody, bodyHead, bodyTail) : _* )
          case e: Elem => Elem(v.prefix, v.label, fixAttrs(v.attributes, "src", v.attributes, true), v.scope, _fixHtml( v.child, inHtml, inHead, justHead, inBody, justBody, bodyHead, bodyTail) : _*)
          case _ => v
        }
        if (_justHead) headChildren += ret
        else if (_justBody && !bodyHead && !bodyTail) bodyChildren += ret
        else if (_bodyHead) addlHead += ret
        else if (_bodyTail) addlTail += ret

        if (bodyHead || bodyTail) Text("")
        else ret
      }
    }
    _fixHtml(xhtml, false, false, false, false, false, false,false)

    val htmlKids = new ListBuffer[Node]

    val nl = Text("\n")

    for {
      node <- HeadHelper.removeHtmlDuplicates(addlHead.toList)
    } {
      headChildren += node
      headChildren += nl
    }

    /**
     * Appends ajax stript to body
     */

    if (LiftRules.autoIncludeAjax(this)) {
      headChildren += <script src={S.encodeURL(contextPath+"/"+
                                               LiftRules.ajaxPath +
                                               "/" + LiftRules.ajaxScriptName())}
        type="text/javascript"/>
      headChildren += nl
    }
  
    val cometList = cometTimes.toList

    /**
     * Appends comet stript reference to head
     */
    if (!cometList.isEmpty &&  LiftRules.autoIncludeComet(this)) {
      headChildren += <script src={S.encodeURL(contextPath+"/"+
                                               LiftRules.cometPath +
                                               "/" + urlEncode(this.uniqueId) +
                                               "/" + LiftRules.cometScriptName())}
        type="text/javascript"/>
      headChildren += nl
    }

    for {
      node <- HeadHelper.removeHtmlDuplicates(addlTail.toList)
    } bodyChildren += node

    bodyChildren += nl

    if (!cometList.isEmpty &&  LiftRules.autoIncludeComet(this)) {
      bodyChildren +=  JsCmds.Script(LiftRules.renderCometPageContents(this, cometList))
      bodyChildren += nl
    }

    if (LiftRules.enableLiftGC) {
      import js._
      import JsCmds._
      import JE._

      bodyChildren += JsCmds.Script(OnLoad(JsRaw("liftAjax.lift_successRegisterGC()")) &
                                    JsCrVar("lift_page", RenderVersion.get))
    }

    htmlKids += nl
    htmlKids += Elem(headTag.prefix, headTag.label, headTag.attributes, headTag.scope, headChildren.toList :_*)
    htmlKids += nl
    htmlKids += Elem(bodyTag.prefix, bodyTag.label, bodyTag.attributes, bodyTag.scope, bodyChildren.toList :_*)
    htmlKids += nl

    val tmpRet = Elem(htmlTag.prefix, htmlTag.label, htmlTag.attributes, htmlTag.scope, htmlKids.toList :_*)

    val ret: Node = if (Props.devMode) {
      LiftRules.xhtmlValidator.toList.flatMap(_(tmpRet)) match {
        case Nil => tmpRet
        case xs =>
          import _root_.scala.xml.transform._

          val errors: NodeSeq = xs.map(e =>
            <div style="border: red solid 2px">
              XHTML Validation error: {e.msg} at line {e.line + 1} and column {e.col}
            </div>)

          val rule = new RewriteRule {
            override def transform(n: Node) = n match {
              case e: Elem if e.label == "body" =>
                Elem(e.prefix, e.label, e.attributes, e.scope,e.child ++ errors :_*)

              case x => super.transform(x)
            }
          }
          (new RuleTransformer(rule)).transform(tmpRet)(0)
      }

    } else tmpRet

    ret
  }
  }

  private[http] def processRequest(request: Req): Box[LiftResponse] = {
    ieMode.is // make sure this is primed
    S.oldNotices(notices)
    LiftSession.onBeginServicing.foreach(f => tryo(f(this, request)))
    val ret = try {
      val sessionDispatch = S.highLevelSessionDispatcher

      val toMatch = request
      NamedPF.applyBox(toMatch, sessionDispatch) match {
        case Full(f) =>
          runParams(request)
          try {
            f() match {
              case Full(r) => Full(checkRedirect(r))
              case _ => LiftRules.notFoundOrIgnore(request, Full(this))
            }
          } finally {
            notices = S.getNotices
          }

        case _ =>
          RenderVersion.get // touch this early

          runParams(request)

          val early = LiftRules.preAccessControlResponse_!!.firstFull(request)

          // Process but make sure we're okay, sitemap wise
          val response: Box[LiftResponse] = early or (request.testLocation match {
              case Left(true) =>
                cleanUpBeforeRender

                PageName(request.uri+" -> "+request.path)

                (request.location.flatMap(_.earlyResponse) or
                 LiftRules.earlyResponse.firstFull(request)) or {
                  ((locTemplate or findVisibleTemplate(request.path, request)).
                   // Phase 1: snippets & templates processing
                   map(xml => processSurroundAndInclude(PageName get, xml)) match {
                      case Full(rawXml: NodeSeq) => {
                          // Phase 2: Head & Tail merge, add additional elements to body & head
                          val xml = merge(rawXml, request)

                          LiftSession.this.synchronized {
                            S.functionMap.foreach {mi =>
                              // ensure the right owner
                              messageCallback(mi._1) = mi._2.owner match {
                                case Empty => mi._2.duplicate(RenderVersion.get)
                                case _ => mi._2
                              }
                            }
                          }

                          notices = Nil
                          // Phase 3: Response conversion including fixHtml
                          Full(LiftRules.convertResponse(xml,
                                                         S.getHeaders(LiftRules.defaultHeaders((xml, request))),
                                                         S.responseCookies,
                                                         request))
                        }
                      case _ => if (LiftRules.passNotFoundToChain) Empty else Full(request.createNotFound)
                    })
                }

              case Right(Full(resp)) => Full(resp)
              case _ if (LiftRules.passNotFoundToChain) => Empty
              case _ if Props.mode == Props.RunModes.Development =>
                Full(ForbiddenResponse("The requested page was not defined in your SiteMap, so access was blocked.  (This message is displayed in development mode only)"))
              case _ => Full(request.createNotFound)
            })

          // Before returning the response check for redirect and set the appropriate state.
          response.map(checkRedirect)
      }
    } catch {
      case ite: _root_.java.lang.reflect.InvocationTargetException if (ite.getCause.isInstanceOf[ResponseShortcutException]) =>
        Full(handleRedirect(ite.getCause.asInstanceOf[ResponseShortcutException], request))

      case rd: _root_.net.liftweb.http.ResponseShortcutException => Full(handleRedirect(rd, request))

      case e => NamedPF.applyBox((Props.mode, request, e), LiftRules.exceptionHandler.toList);

    }

    LiftSession.onEndServicing.foreach(f => tryo(f(this, request, ret)))
    ret
  }

  private def cleanUpBeforeRender {
    // Reset the mapping between ID and Style for Ajax notices.
    MsgErrorMeta(new HashMap)
    MsgWarningMeta(new HashMap)
    MsgNoticeMeta(new HashMap)
  }

  private[http] def handleRedirect(re: ResponseShortcutException, request: Req): LiftResponse = {
    if (re.doNotices) notices = S.getNotices

    re.response
  }

  /**
   * Set a session-local variable to a value
   *
   * @param name -- the name of the variable
   * @param value -- the value of the variable
   */
  private [liftweb] def set[T](name: String, value: T): Unit = sessionVarSync.synchronized {
    myVariables = myVariables + (name -> value)
  }

  /**
   * Gets the named variable if it exists
   *
   * @param name -- the name of the session-local variable to get
   *
   * @return Full(value) if found, Empty otherwise
   */
  private [liftweb] def get[T](name: String): Box[T] = sessionVarSync.synchronized {
    Box(myVariables.get(name)).asInstanceOf[Box[T]]
  }

  /**
   * Unset the named variable
   *
   * @param name the variable to unset
   */
  private [liftweb] def unset(name: String): Unit = sessionVarSync.synchronized {
    myVariables -= name
  }


  private[http] def attachRedirectFunc(uri: String, f : Box[() => Unit]) = {
    f map { fnc =>
      val func: String = LiftSession.this.synchronized {
        val funcName = Helpers.nextFuncName
        messageCallback(funcName) = S.NFuncHolder(() => {
            try {
              fnc()
            } finally {
              LiftSession.this.synchronized {
                messageCallback -= funcName
              }
            }
          })
        funcName
      }
      val sep = uri contains("?") match {case true => "&" case _ => "?"}
      uri + sep + func +"=_"
    } openOr uri

  }

  private[http] def checkRedirect(resp: LiftResponse): LiftResponse = resp match {
    case r: RedirectWithState =>
      val uri = r.uri
    val state = r.state
    val cookies = r.cookies
      state.msgs.foreach(m => S.message(m._1, m._2))
      notices = S.getNotices
      RedirectResponse(attachRedirectFunc(uri, state.func), cookies:_*)
    case _ => resp
  }


  private def allElems(in: NodeSeq, f: Elem => Boolean): List[Elem] = {
    val lb = new ListBuffer[Elem]

    def appendAll(in: NodeSeq, lb: ListBuffer[Elem]) {
      in.foreach{
        case Group(ns) => appendAll(ns, lb)
        case e: Elem if f(e) => lb += e; appendAll(e.child, lb)
        case e: Elem => appendAll(e.child, lb)
        case _ =>
      }
    }
    appendAll(in, lb)

    lb.toList
  }

  private def findVisibleTemplate(path: ParsePath, session: Req): Box[NodeSeq] = {
    val tpath = path.partPath
    val splits = tpath.toList.filter {a => !a.startsWith("_") && !a.startsWith(".") && a.toLowerCase.indexOf("-hidden") == -1} match {
      case s @ _ if (!s.isEmpty) => s
      case _ => List("index")
    }
    findAnyTemplate(splits, S.locale)
  }

  private[liftweb] def findTemplate(name: String): Box[NodeSeq] = {
    val splits = (if (name.startsWith("/")) name else "/"+name).split("/").toList.drop(1) match {
      case Nil => List("index")
      case s => s
    }

    findAnyTemplate("templates-hidden" :: splits, S.locale) match {
      case Full(x) => Full(x)
      case f: Failure if Props.devMode => f
      case _ => findAnyTemplate(splits, S.locale)
    }
  }

  private object snippetMap extends RequestVar[Map[String, AnyRef]](Map())

  private def findSnippetClass(name: String): Box[Class[AnyRef]] = {
    if (name == null) Empty
    else findClass(name, LiftRules.buildPackage("snippet") ::: ("lift.app.snippet" :: "net.liftweb.builtin.snippet" :: Nil))
  }

  private def instantiateOrRedirect[T](c: Class[T]): Box[T] = tryo({
      case e: ResponseShortcutException => throw e
      case ite: _root_.java.lang.reflect.InvocationTargetException
        if (ite.getCause.isInstanceOf[ResponseShortcutException]) => throw ite.getCause.asInstanceOf[ResponseShortcutException]
    }, c.newInstance)

  private def findAttributeSnippet(name: String, rest: MetaData): MetaData = {
    S.doSnippet(name) {
      val (cls, method) = splitColonPair(name, null, "render")

      findSnippetClass(cls).flatMap(clz => instantiateOrRedirect(clz).flatMap(inst =>
          (invokeMethod(clz, inst, method) match {
              case Full(md: MetaData) => Full(md.copy(rest))
              case _ => Empty
            }))).openOr(rest)
    }
  }

  /**
   * Finds a template named name and then runs it throught the Lift processing engine
   */
  def findAndProcessTemplate(name: List[String]): Box[Elem] = {
    def findElem(in: NodeSeq): Box[Elem] =
    Box(in.toList.flatMap {
      case e: Elem => Some(e)
      case _ => None
    } headOption)
	
    for {
      template <- findAnyTemplate(name, S.locale) ?~ ("Template "+name+" not found")
      res <- findElem(processSurroundAndInclude(name.mkString("/", "/", ""), template))
    } yield res
  }

  private def processAttributes(in: MetaData) : MetaData = {
    in match {
      case Null => Null
      case mine: PrefixedAttribute if (mine.pre == "lift") => {
          mine.key match {
            case "snippet" => findAttributeSnippet(mine.value.text, processAttributes(in.next))
            case _ => mine.copy(processAttributes(in.next))
          }
        }
      case notMine => notMine.copy(processAttributes(in.next))
    }
  }

  private def findSnippetInstance(cls: String): Box[AnyRef] =
  S.snippetForClass(cls) or
  (findSnippetClass(cls).flatMap(c => instantiateOrRedirect(c)) match {
      case Full(inst: StatefulSnippet) =>
        inst.snippetName = cls; S.setSnippetForClass(cls, inst); Full(inst)
      case Full(ret) => Full(ret)
      case fail : Failure => fail
      case _ => Empty
    })


  private def reportSnippetError(page: String,
                                 snippetName: Box[String],
                                 why: LiftRules.SnippetFailures.Value,
                                 whole: NodeSeq): NodeSeq =
  {
    for {
      f <- LiftRules.snippetFailedFunc.toList
    }
    f(LiftRules.SnippetFailure(page, snippetName, why))

    Props.mode match {
      case Props.RunModes.Development =>
        <div style="display: block; margin: 8px; border: 2px solid red">
          Error processing snippet {snippetName openOr "N/A"}.  Reason: {why}
          XML causing this error:
          <br/>
          <pre>
            {whole.toString}
          </pre>
          <i>note: this error is displayed in the browser because
            your application is running in "development" mode.  If you
            set the system property run.mode=production, this error will not
            be displayed, but there will be errors in the output logs.
          </i>
        </div>
      case _ => NodeSeq.Empty
    }
  }


  private def processSnippet(page: String, snippetName: Box[String],
                             attrs: MetaData,
                             wholeTag: NodeSeq,
                             passedKids: NodeSeq): NodeSeq = {
    val isForm = !attrs.get("form").toList.isEmpty

    val eagerEval: Boolean = attrs.get("eager_eval").map(toBoolean) getOrElse false

    val kids = if (eagerEval) processSurroundAndInclude(page, passedKids) else passedKids

    def locSnippet(snippet: String): Box[NodeSeq] =
    for (req <- S.request;
         loc <- req.location;
         func <- loc.snippet(snippet)) yield func(kids)

    def locateAndCacheSnippet(cls: String): Box[AnyRef] =
    snippetMap.is.get(cls) or {
      val ret = LiftRules.snippet(cls) or findSnippetInstance(cls)
      ret.foreach(s => snippetMap.set(snippetMap.is + (cls -> s)))
      ret
    }

    val ret: NodeSeq = snippetName.map(snippet =>
      S.doSnippet(snippet)(
        (S.locateMappedSnippet(snippet).map(_(kids)) or
         locSnippet(snippet)).openOr(
          S.locateSnippet(snippet).map(_(kids)) openOr {
            val (cls, method) = splitColonPair(snippet, null, "render")
            (locateAndCacheSnippet(cls)) match {

              case Full(inst: StatefulSnippet) =>
                if (inst.dispatch.isDefinedAt(method)) {
                  val res = inst.dispatch(method)(kids)

                  (if (isForm && !res.isEmpty) SHtml.hidden(() => inst.registerThisSnippet) else NodeSeq.Empty) ++
                  res
                } else reportSnippetError(page, snippetName,
                                          LiftRules.SnippetFailures.StatefulDispatchNotMatched,
                                          wholeTag)

              case Full(inst: DispatchSnippet) =>
                if (inst.dispatch.isDefinedAt(method)) inst.dispatch(method)(kids)
                else reportSnippetError(page, snippetName,
                                        LiftRules.SnippetFailures.StatefulDispatchNotMatched,
                                        wholeTag)

              case Full(inst) => {
                  val ar: Array[AnyRef] = List(Group(kids)).toArray
                  ((invokeMethod(inst.getClass, inst, method, ar)) or invokeMethod(inst.getClass, inst, method)) match {
                    case CheckNodeSeq(md) => md
                    case it =>
                      reportSnippetError(page, snippetName,
                                         LiftRules.SnippetFailures.MethodNotFound,
                                         wholeTag)
                  }
                }
              case Failure(_, Full(exception), _) => Log.warn("Snippet instantiation error", exception)
                reportSnippetError(page, snippetName,
                                   LiftRules.SnippetFailures.InstantiationException,
                                   wholeTag)


              case _ => reportSnippetError(page, snippetName,
                                           LiftRules.SnippetFailures.ClassNotFound,
                                           wholeTag)

            }
          }))).openOr{
      reportSnippetError(page, snippetName,
                         LiftRules.SnippetFailures.NoNameSpecified,
                         wholeTag)
    }

    def checkMultiPart(in: MetaData): MetaData = in.filter(_.key == "multipart").toList match {
      case Nil => Null
      case x => new UnprefixedAttribute("enctype", Text("multipart/form-data"), Null)
    }

    def checkAttr(attr_name: String, in: MetaData, base: MetaData): MetaData =
    in.filter(_.key == attr_name).toList match {
      case Nil => base
      case x => new UnprefixedAttribute(attr_name, Text(x.head.value.text),
                                        base)
    }

    if (ret.isEmpty) ret else
    attrs.get("form").map(ft => (
        (<form action={S.uri} method={ft.text.trim.toLowerCase}>{ret}</form> %
         checkMultiPart(attrs)) % LiftRules.formAttrs.vend.foldLeft[MetaData](Null)((base, name) => checkAttr(name, attrs, base))
      )) getOrElse ret

  }


  /**
   * Apply HTML specific corrections such as adding the context path etc.
   */
  def fixHtml(in: NodeSeq): NodeSeq = Req.fixHtml(contextPath, in)


  /**
   * The partial function that defines how lift tags are processed for this session.  Initially composed
   * of LiftRules.liftTagProcessing orElse the default lift tag processing.  If you need to change the
   * way a particular session handles lift tags, alter this partial function.
   */
  var liftTagProcessing: List[LiftRules.LiftTagPF] = _

  /**
   * The basic partial function that does lift tag processing
   */
  private def _defaultLiftTagProcessing: LiftRules.LiftTagPF =
  NamedPF("Default Lift Tags") {
    case ("snippet", elm, metaData, kids, page) =>
      metaData.get("type") match {
        case Some(tn) =>
          S.doSnippet(tn.text){
            NamedPF((tn.text, elm, metaData, kids, page),
                    liftTagProcessing)
          }

        case _ => processSnippet(page, Empty , elm.attributes, elm, elm.child)
      }
    case (snippetInfo, elm, metaData, kids, page) =>
      processSnippet(page, Full(snippetInfo), metaData, elm, kids)
  }

  liftTagProcessing = LiftRules.liftTagProcessing.toList ::: List(_defaultLiftTagProcessing)

  private def asNodeSeq(in: Seq[Node]): NodeSeq = in

  /**
   * Processes the surround tag and other lift tags
   */
  def processSurroundAndInclude(page: String, in: NodeSeq): NodeSeq =
  in.flatMap{
    v =>
    v match {
      case Group(nodes) =>
        Group(processSurroundAndInclude(page, nodes))

      case elm: Elem if elm.prefix == "lift" || elm.prefix == "l"=>
        S.doSnippet(elm.label){
          S.withAttrs(elm.attributes) {
            S.setVars(elm.attributes){
              processSurroundAndInclude(page, NamedPF((elm.label, elm, elm.attributes,
                                                       asNodeSeq(elm.child), page),
                                                      liftTagProcessing))
            }}}

      case elm: Elem =>
        Elem(v.prefix, v.label, processAttributes(v.attributes),
             v.scope, processSurroundAndInclude(page, v.child) : _*)
      case _ => v
    }
  }

  /**
   * A nicely named proxy for processSurroundAndInclude.  This method processes
   * a Lift template
   *
   * @param pageName -- the name of the page being processed (for error reporting)
   * @param template -- the template to process using Lift's templating engine
   */
  def runTemplate(pageName: String, template: NodeSeq): NodeSeq =
  processSurroundAndInclude(pageName, template)

  /**
   * Finds all Comet actors by type
   */
  def findComet(theType: String): List[CometActor] = synchronized {
    asyncComponents.filter{case ((Full(name), _), _) => name == theType case _ => false}.toList.map{case (_, value) => value}
  }

  private object cometSetup extends RequestVar[List[((Box[String], Box[String]), Any)]](Nil)

  /**
   * Allows you to send messages to a CometActor that may or may not be set up yet
   */
  def setupComet(theType: String, name: Box[String], msg: Any) {
    cometSetup((Full(theType) -> name, msg) :: cometSetup.is)
  }

  private[liftweb] def findComet(theType: Box[String], name: Box[String], defaultXml: NodeSeq, attributes: Map[String, String]): Box[CometActor] =
  {
    val what = (theType -> name)
    val ret = synchronized {

      val ret = Box(asyncComponents.get(what)).or( {
          theType.flatMap{
            tpe =>
            val ret = findCometByType(tpe, name, defaultXml, attributes)
            ret.foreach(r =>
              synchronized {
                asyncComponents(what) = r
                asyncById(r.uniqueId) = r
              })
            ret
          }
        })

      ret
    }

    for {
      actor <- ret
      (cst, csv) <- cometSetup.is if cst == what
    } actor ! csv

    ret
  }


  /**
   * Finds a Comet actor by ID
   */
  def getAsyncComponent(id: String): Box[CometActor] = synchronized(asyncById.get(id))

  /**
   * Adds a new COmet actor to this session
   */
  private[http] def addCometActor(act: CometActor): Unit = synchronized {
    asyncById(act.uniqueId) = act
  }

  /**
   * Remove a Comet actor
   */
  private [http] def removeCometActor(act: CometActor): Unit = synchronized {
    asyncById -= act.uniqueId
    messageCallback -= act.jsonCall.funcId
    asyncComponents -= (act.theType -> act.name)
    val id = Full(act.uniqueId)
    messageCallback.keysIterator.toList.foreach{
      k =>
      val f = messageCallback(k)
      if (f.owner == id) {
        messageCallback -= k
      }
    }
  }

  private def findCometByType(contType: String, name: Box[String], defaultXml: NodeSeq, attributes: Map[String, String]): Box[CometActor] = {
    findType[CometActor](contType, LiftRules.buildPackage("comet") ::: ("lift.app.comet" :: Nil)).flatMap{
      cls =>
      tryo((e: Throwable) => e match {case e: _root_.java.lang.NoSuchMethodException => ()
          case e => Log.info("Comet find by type Failed to instantiate "+cls.getName, e)}) {
        val constr = cls.getConstructor()
        val ret = constr.newInstance().asInstanceOf[CometActor]
        ret.initCometActor(this, Full(contType), name, defaultXml, attributes)

        // ret.link(this)
        ret ! PerformSetupComet
        ret.asInstanceOf[CometActor]
      }  or tryo((e: Throwable) => Log.info("Comet find by type Failed to instantiate "+cls.getName, e)) {
        val constr = cls.getConstructor(this.getClass , classOf[Box[String]], classOf[NodeSeq], classOf[Map[String, String]])
        val ret = constr.newInstance(this, name, defaultXml, attributes).asInstanceOf[CometActor];
        ret.start
        // ret.link(this)
        ret ! PerformSetupComet
        ret.asInstanceOf[CometActor]
      }
    }
  }

  private def failedFind(in: Failure): NodeSeq =
  <html  xmlns:lift="http://liftweb.net" xmlns="http://www.w3.org/1999/xhtml"><head/>
    <body><div style="border: 1px red solid">Error locating template.  Message: {in.msg} <br/>
        {
          in.exception.map(e => <pre>{e.toString}
              {e.getStackTrace.map(_.toString).mkString("\n")}
                                </pre>).openOr(NodeSeq.Empty)
        }
        This message is displayed because you are in Development mode.
          </div></body></html>

  private[liftweb] def findAndMerge(templateName: Box[Seq[Node]], atWhat: Map[String, NodeSeq]): NodeSeq = {
    val name = templateName.map(s => if (s.text.startsWith("/")) s.text else "/"+ s.text).openOr("/templates-hidden/default")

    findTemplate(name) match {
      case f@ Failure(msg, be, _) if Props.devMode =>
        failedFind(f)
      case Full(s) => bind(atWhat, s)
      case _ => atWhat.valuesIterator.flatMap(_.iterator).toList
    }
  }

}


/**
 * The response from a page saying that it's been rendered
 */
case object ShutDown

/**
 * If a class is to be used as a lift view (rendering from code rather than a static template)
 * and the method names are to be used as "actions", the view must be marked as "InsecureLiftView"
 * because there exists the ability to execute arbitrary methods based on wire content
 */
trait InsecureLiftView

/**
 *  The preferred way to do lift views... implement a partial function that dispatches
 * the incoming request to an appropriate method
 */
trait LiftView {
  implicit def nsToCns(in: NodeSeq): Box[NodeSeq] = Box.legacyNullTest(in)
  def dispatch : PartialFunction[String, () => Box[NodeSeq]]
}

/**
 * Contains functions for obtaining templates
 */
object TemplateFinder {
  private val suffixes = List("", "html", "xhtml", "htm")

  import LiftRules.ViewDispatchPF

  private def checkForLiftView(part: List[String], last: String, what: ViewDispatchPF): Box[NodeSeq] = {
    if (what.isDefinedAt(part)) {
      what(part) match {
        case Right(lv) => if (lv.dispatch.isDefinedAt(last)) lv.dispatch(last)() else Empty
        case _ => Empty
      }
    } else Empty
  }

  private def checkForFunc(whole: List[String], what: ViewDispatchPF): Box[NodeSeq] =
  if (what.isDefinedAt(whole)) what(whole) match {
    case Left(func) => func()
    case _ => Empty
  }
  else Empty

  private def findInViews(whole: List[String], part: List[String],
                          last: String,
                          what: List[ViewDispatchPF]): Box[NodeSeq] =
  what match {
    case Nil => Empty
    case x :: xs =>
      (checkForLiftView(part, last, x) or checkForFunc(whole, x)) match {
        case Full(ret) => Full(ret)
        case _ => findInViews(whole, part, last, xs)
      }
  }

  /**
   * Given a list of paths (e.g. List("foo", "index")),
   * find the template.
   * @param places - the path to look in
   *
   * @return the template if it can be found
   */
  def findAnyTemplate(places: List[String]): Box[NodeSeq] =
  findAnyTemplate(places, S.locale)

  /**
   * Given a list of paths (e.g. List("foo", "index")),
   * find the template.
   * @param places - the path to look in
   * @param locale - the locale of the template to search for
   *
   * @return the template if it can be found
   */
  def findAnyTemplate(places: List[String], locale: Locale): Box[NodeSeq] = {
    /*
     From a Scala coding standpoint, this method is ugly.  It's also a performance
     hotspot that needed some tuning.  I've made the code very imperative and
     tried to make sure there are no anonymous functions created in this method.
     The few extra lines of code and the marginal reduction in readibility should
     yield better performance.  Please don't change this method without chatting with
     me first.  Thanks!  DPP
     */
    val lrCache = LiftRules.templateCache
    val cache = if (lrCache.isDefined) lrCache.open_! else NoCache

    val key = (locale, places)
    val tr = cache.get(key)

    if (tr.isDefined) tr
    else
    {
      val part = places.dropRight(1)
      val last = places.last

      findInViews(places, part, last, LiftRules.viewDispatch.toList) match {
        case Full(lv) =>
          Full(lv)

        case _ =>
          val pls = places.mkString("/","/", "")

          val se = suffixes.iterator
          val sl = List("_"+locale.toString, "_"+locale.getLanguage, "")

          var found = false
          var ret: NodeSeq = null

          while (!found && se.hasNext) {
            val s = se.next
            val le = sl.iterator
            while (!found && le.hasNext) {
              val p = le.next
              val name = pls + p + (if (s.length > 0) "." + s else "")
              val rb = LiftRules.finder(name)
              if (rb.isDefined) {
                import scala.xml.dtd.ValidationException
                val xmlb = try {
                  PCDataXmlParser(rb.open_!)
                } catch {
                  case e: ValidationException if Props.devMode =>
                    return(Full(<div style="border: 1px red solid">Error locating template {name}.<br/>  Message: {e.getMessage} <br/>
                          {
                            <pre>
                              {e.toString}
                              {e.getStackTrace.map(_.toString).mkString("\n")}
                            </pre>
                          }
                          This message is displayed because you are in Development mode.
                                </div>))

                  case e: ValidationException => Empty
                }
                if (xmlb.isDefined) {
                  found = true
                  ret = (cache(key) = xmlb.open_!)
                } else if (xmlb.isInstanceOf[Failure] && Props.devMode) {
                  val msg = xmlb.asInstanceOf[Failure].msg
                  val e = xmlb.asInstanceOf[Failure].exception
                  return(Full(<div style="border: 1px red solid">Error locating template {name}.<br/>  Message: {msg} <br/>
                        {
                          {e match {
                              case Full(e) =>
                                <pre>
                                  {e.toString}
                                  {e.getStackTrace.map(_.toString).mkString("\n")}
                                </pre>
                              case _ => NodeSeq.Empty
                            }
                          }
                        }
                        This message is displayed because you are in Development mode.
                              </div>))
                }
              }
            }
          }

          if (found) Full(ret)
          else lookForClasses(places)
      }
    }
  }

  private def lookForClasses(places : List[String]): Box[NodeSeq] = {
    val (controller, action) = places match {
      case ctl :: act :: _ => (ctl, act)
      case ctl :: _ => (ctl, "index")
      case Nil => ("default_template", "index")
    }
    val trans = List[String => String](n => n, n => camelCase(n))
    val toTry = trans.flatMap(f => (LiftRules.buildPackage("view") ::: ("lift.app.view" :: Nil)).map(_ + "."+f(controller)))

    first(toTry) {
      clsName =>
      try {
        tryo(List(classOf[ClassNotFoundException]), Empty) (Class.forName(clsName).asInstanceOf[Class[AnyRef]]).flatMap{
          c =>
          (c.newInstance match {
              case inst: InsecureLiftView => c.getMethod(action).invoke(inst)
              case inst: LiftView if inst.dispatch.isDefinedAt(action) => inst.dispatch(action)()
              case _ => Empty
            }) match {
            case null | Empty | None => Empty
            case n: Group => Full(n)
            case n: Elem => Full(n)
            case s: NodeSeq => Full(s)
            case Some(n: Group) => Full(n)
            case Some(n: Elem) => Full(n)
            case Some(n: NodeSeq) => Full(n)
            case Some(SafeNodeSeq(n)) => Full(n)
            case Full(n: Group) => Full(n)
            case Full(n: Elem) => Full(n)
            case Full(n: NodeSeq) => Full(n)
            case Full(SafeNodeSeq(n)) => Full(n)
            case _ => Empty
          }
        }
      } catch {
        case ite: _root_.java.lang.reflect.InvocationTargetException if (ite.getCause.isInstanceOf[ResponseShortcutException]) => throw ite.getCause
        case re: ResponseShortcutException => throw re
        case _ => Empty
      }
    }
  }
}

