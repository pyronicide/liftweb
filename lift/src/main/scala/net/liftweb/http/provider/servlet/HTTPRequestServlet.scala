package net.liftweb
package http
package provider
package servlet

import _root_.java.io.{InputStream}
import _root_.java.util.{Locale}
import _root_.javax.servlet.http.{HttpServletRequest}
import _root_.org.apache.commons.fileupload.servlet._
import _root_.org.apache.commons.fileupload.ProgressListener
import _root_.net.liftweb.util._
import Helpers._

class HTTPRequestServlet(req: HttpServletRequest) extends HTTPRequest {

  private val ctx = new HTTPServletContext(req.getSession.getServletContext)

  private val (hasContinuations_?, contSupport, getContinuation, getObject, setObject, suspend, resume) = {
    try {
      val cc = Class.forName("org.mortbay.util.ajax.ContinuationSupport")
      val meth = cc.getMethod("getContinuation", classOf[HTTPRequest], classOf[AnyRef])
      val cci = Class.forName("org.mortbay.util.ajax.Continuation")
      val getObj = cci.getMethod("getObject")
      val setObj = cci.getMethod("setObject", classOf[AnyRef])
      val suspend = cci.getMethod("suspend", _root_.java.lang.Long.TYPE)
      val resume = cci.getMethod("resume")
      (true, (cc), (meth), (getObj), (setObj), (suspend), resume)
    } catch {
      case e => (false, null, null, null, null, null, null)
    }
  }


  def cookies: List[HTTPCookie] =
    (Box !! req.getCookies).map(_.toList.map(c => HTTPCookie(c.getName,
                                              Box !! (c.getValue),
                                              Box !! (c.getDomain),
                                              Box !! (c.getPath),
                                              Box !! (c.getMaxAge),
                                              Box !! (c.getVersion),
                                              Box !! (c.getSecure)))) openOr Nil

  def authType: Box[String] = Box !! req.getAuthType

  def headers(name: String): List[String] = enumToList[String](req.getHeaders(name).asInstanceOf[_root_.java.util.Enumeration[String]])

  lazy val headers: List[HTTPParam] = enumToList[String](req.getHeaderNames.asInstanceOf[_root_.java.util.Enumeration[String]]).
    map(n => HTTPParam(n, headers(n)))

  def contextPath: String = req.getContextPath

  def context: HTTPContext = ctx

  def contentType = Box !! req.getContentType

  def session = new HTTPServletSession(req getSession)

  def uri = req.getRequestURI

  def url = req.getRequestURL.toString

  def queryString: Box[String] = Box !! req.getQueryString

  def param(name: String): List[String] = req.getParameterValues(name).toList

  def params: List[HTTPParam] = enumToList[String](req.getParameterNames.asInstanceOf[_root_.java.util.Enumeration[String]]).
    map(n => HTTPParam(n, param(n)))

  def paramNames: List[String] = params map(_.name)

  def remoteAddress: String = req.getRemoteAddr

  def remotePort: Int = req.getRemotePort

  def remoteHost: String = req.getRemoteHost

  def serverName = req getServerName

  def scheme: String = req getScheme

  def serverPort = req getServerPort

  def method: String = req.getMethod

  def locale: Box[Locale] = Box !! req.getLocale

  def inputStream: InputStream = req.getInputStream

  def multipartContent_? = ServletFileUpload.isMultipartContent(req)

  /**
  * @return the sessionID (if there is one) for this request.  This will *NOT* create
  * a new session if one does not already exist
  */
  def sessionId: Box[String] =
  for {
    httpSession <- Box !! req.getSession(false)
    id <- Box !! httpSession.getId
  } yield id

  def extractFiles: List[ParamHolder] = (new Iterator[ParamHolder] {
    val mimeUpload = (new ServletFileUpload)
    mimeUpload.setProgressListener(new ProgressListener{
       lazy val progList: (Long, Long, Int) => Unit = S.session.flatMap(_.progressListener) openOr LiftRules.progressListener

       def update(a: Long, b: Long, c: Int) {progList(a,b,c)}
    })

    mimeUpload.setSizeMax(LiftRules.maxMimeSize)
    mimeUpload.setFileSizeMax(LiftRules.maxMimeFileSize)
    val what = mimeUpload.getItemIterator(req)

    def hasNext = what.hasNext
    def next = what.next match {
      case f if (f.isFormField) => NormalParamHolder(f.getFieldName, new String(readWholeStream(f.openStream), "UTF-8"))
      case f => LiftRules.handleMimeFile(f.getFieldName, f.getContentType, f.getName, f.openStream)
    }}).toList

  def hasSuspendResumeSupport_? =
    if (!hasContinuations_?) None
    else if (Props.inGAE) None
    else {
      val cont = getContinuation.invoke(contSupport, req, LiftRules)
      val ret = getObject.invoke(cont)
      try {
        setObject.invoke(cont, null)
        Some(ret)
      }
      catch {
        case e: Exception => None
      }
    }


  def suspend(timeout: Long): Nothing = {
    try {
      val cont = getContinuation.invoke(contSupport, req, LiftRules)
      Log.trace("About to suspend continuation")
      suspend.invoke(cont, new _root_.java.lang.Long(timeout))
      throw new Exception("Bail")
    } catch {
      case e: _root_.java.lang.reflect.InvocationTargetException if e.getCause.getClass.getName.endsWith("RetryRequest") =>
        throw e.getCause
    }
  }

  def resume(what: AnyRef) {
    val cont = getContinuation.invoke(contSupport, req, LiftRules)
    setObject.invoke(cont, what)
    resume.invoke(cont)
  }

  def setCharacterEncoding(encoding: String) = req.setCharacterEncoding(encoding)


}
