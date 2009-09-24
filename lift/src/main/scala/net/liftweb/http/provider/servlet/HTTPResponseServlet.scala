package net.liftweb
package http
package provider
package servlet


import _root_.scala.collection.mutable.{ListBuffer}
import _root_.java.io.{OutputStream}
import _root_.javax.servlet.http.{HttpServletResponse, Cookie}
import _root_.net.liftweb.util._
import Helpers._

class HTTPResponseServlet(resp: HttpServletResponse) extends HTTPResponse {

  def addCookies(cookies: List[HTTPCookie]) = cookies.foreach {
    case c =>
      val cookie = new javax.servlet.http.Cookie(c.name, c.value openOr null)
      c.domain map (cookie.setDomain(_))
      c.path map (cookie.setPath(_))
      c.maxAge map (cookie.setMaxAge(_))
      c.version map (cookie.setVersion(_))
      c.secure_? map (cookie.setSecure(_))
      resp.addCookie(cookie)
  }

  def encodeUrl(url: String): String = resp encodeURL url

  def addHeaders(headers: List[HTTPParam]): Unit =
    for {
      h <- headers
      value <- h.values
    } resp.addHeader(h.name, value)


  def setStatus(status: Int) = resp setStatus status

  def outputStream: OutputStream = resp getOutputStream
}
