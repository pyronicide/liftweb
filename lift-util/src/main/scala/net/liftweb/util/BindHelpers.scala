package net.liftweb.util

/*
 * Copyright 2007-2009 WorldWide Conferencing, LLC
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

import _root_.scala.xml.{NodeSeq, Node, SpecialNode, Text, Elem,
                         Group, MetaData, Null, UnprefixedAttribute,
                         PrefixedAttribute}

/**
 * This trait is used to identify an object that is representable as a {@link NodeSeq}.
 */
trait Bindable {
  def asHtml: NodeSeq
}

trait AttrHelper[+Holder[X]] {
  type Info

  def apply(key: String): Holder[Info] = convert(findAttr(key))
  def apply(prefix: String, key: String): Holder[Info] =
  convert(findAttr(prefix, key))

  def apply(key: String, default: => Info): Info =
  findAttr(key) getOrElse default

  def apply(prefix: String, key: String, default: => Info): Info =
  findAttr(prefix, key) getOrElse default

  def apply[T](key: String, f: Info => T): Holder[T] =
  convert(findAttr(key).map(f))

  def apply[T](prefix: String, key: String, f: Info => T): Holder[T] =
  convert(findAttr(prefix, key).map(f))
  def apply[T](key: String, f: Info => T, default: => T): T =
  findAttr(key).map(f) getOrElse default

  def apply[T](prefix: String, key: String, f: Info => T, default: => T): T =
  findAttr(prefix, key).map(f) getOrElse default

  protected def findAttr(key: String): Option[Info]
  protected def findAttr(prefix: String, key: String): Option[Info]
  protected def convert[T](in: Option[T]): Holder[T]
}

/**
 * BindHelpers can be used to obtain additional information while a {@link bind} call is executing.
 * This informaiton includes node attributes of the current bound node or the entire NodeSeq that is
 * to be bound. Since the context is created during bind execution and destroyed when bind terminates,
 * you can benefit of these helpers in the context of FuncBindParam or FuncAttrBindParam. You can
 * also provide your own implementation of BindParam and your BindParam#calcValue function will be called
 * in the appropriate context.
 *
 * Example:
 * <pre>
 * bind("hello", xml,
 *      "someNode" -> {node: NodeSeq => <function-body>})
 * </pre>
 *
 * In <code>function-body</code> you can safely use BindHelpers methods to obtain correctly-scoped information.
 */
object BindHelpers extends BindHelpers {

  private val _bindNodes = new ThreadGlobal[List[NodeSeq]]
  private val _currentNode = new ThreadGlobal[Elem]

  /**
   * A list of NodeSeq that preceeds the NodeSeq passed to bind. The head of the list
   * is the most recent NodeSeq. This returns Empty if it is called outside its context,
   * or Full(Nil) if there are no child nodes but the function is called within the
   * appropriate context.
   */
  def bindNodes: Box[List[NodeSeq]] = _bindNodes.box

  /**
   * A Box containing the current Elem, the children of which are passed to the bindParam
   */
  def currentNode: Box[Elem] = _currentNode.box

  /**
   * Helpers for obtaining attributes of the current Elem
   */
  object attr extends AttrHelper[Option] {
    type Info = NodeSeq

    protected def findAttr(key: String): Option[Info] =
    for {n  <- _currentNode.box.toOption
         at <- n.attributes.find(at => at.key == key && !at.isPrefixed)}
    yield at.value

    protected def findAttr(prefix: String, key: String): Option[Info] =
    for {n  <- _currentNode.box.toOption
         at <- n.attributes.find {
        case at: PrefixedAttribute => at.key == key && at.pre == prefix
        case _ => false
      }}
    yield at.value

    protected def convert[T](in: Option[T]): Option[T] = in

  }
}

/**
 * Helpers assocated with bindings
 */
trait BindHelpers {

  /**
   * Takes attributes from the first node of 'in' (if any) and mixes
   * them into 'out'. Curried form can be used to produce a
   * NodeSeq => NodeSeq for bind.
   *
   * @param in where to take the attributes from
   * @param out where to put the attributes
   *
   * @return 'out' element with attributes from 'in'
   */
  def mixinAttributes(out: Elem)(in: NodeSeq): NodeSeq = {
    val attributes = in.headOption.map(_.attributes).getOrElse(Null)
    out % attributes
  }

  /**
   * Finds and returns one of many templates from the children based
   * upon the namespace and tag name: for example, for prefix "choose"
   * and tag name "stuff" this would return the contents of the
   * first tag <code>&lt;choose:stuff&gt; ... &lt;/choose:stuff&gt;</code>
   * in the specified NodeSeq.
   *
   * @param prefix the prefix (e.g., "choose")
   * @param tag the tag to choose (e.g., "stuff")
   * @param xhtml the node sequence to search for the specified element
   *
   * @return the first matching node sequence
   */
  def chooseTemplate(prefix: String, tag: String, xhtml: NodeSeq): NodeSeq =
  Helpers.findElems(xhtml)(e => e.label == tag && e.prefix == prefix).toList match {
    case Nil => NodeSeq.Empty
    case x :: xs => x.child
  }

  /**
   * Similar to chooseTemplate, this returns the contents of the element in a Full Box if
   * found or an Empty Box otherwise.
   */
  def template(xhtml: NodeSeq, prefix: String, tag: String): Box[NodeSeq] =
  Helpers.findElems(xhtml)(e => e.label == tag && e.prefix == prefix).toList match {
    case Nil => Empty
    case x :: xs => Full(x.child)
  }

  /**
   * Find two of many templates from the children
   */
  def template(xhtml: NodeSeq, prefix: String, tag1: String,
               tag2: String): Box[(NodeSeq, NodeSeq)] =
  for (x1 <- template(xhtml, prefix, tag1);
       x2 <- template(xhtml, prefix, tag2)) yield (x1, x2)

  /**
   * Find three of many templates from the children
   */
  def template(xhtml: NodeSeq, prefix: String, tag1: String,
               tag2: String, tag3: String): Box[(NodeSeq, NodeSeq, NodeSeq)] =
  for (x1 <- template(xhtml, prefix, tag1);
       x2 <- template(xhtml, prefix, tag2);
       x3 <- template(xhtml, prefix, tag3)) yield (x1, x2, x3)

  /**
   * Base class for Bind parameters. A bind parameter has a name and is able to extract its value from a NodeSeq.
   */
  sealed trait BindParam {
    def name: String
    def calcValue(in: NodeSeq): NodeSeq
  }

  trait BindWithAttr {
    def newAttr: String
  }

  /**
   * Constant BindParam always returning the same value
   */
  final class TheBindParam(val name: String,val value: NodeSeq) extends Tuple2(name, value) with BindParam {
    def calcValue(in: NodeSeq): NodeSeq = value
  }

  /**
   * Constant BindParam always returning the same value
   */
  final class TheStrBindParam(val name: String,val value: String) extends Tuple2(name, value) with BindParam {
    def calcValue(in: NodeSeq): NodeSeq = Text(value)
  }

  /**
   * BindParam taking its value from an attribute
   */
  final class AttrBindParam(val name: String,val myValue: NodeSeq,
                                 val newAttr: String) extends BindParam with BindWithAttr {
    def calcValue(in: NodeSeq): NodeSeq = myValue
  }

  /**
   * BindParam using a function to calculate its value
   */
  final class FuncBindParam(val name: String, val value: NodeSeq => NodeSeq) extends Tuple2(name, value) with BindParam {
    def calcValue(in: NodeSeq): NodeSeq = value(in)
  }

  /**
   * BindParam using a function to calculate its value
   */
  final class FuncAttrBindParam(val name: String,val value: NodeSeq => NodeSeq, val newAttr: String) extends BindParam with BindWithAttr {
    def calcValue(in: NodeSeq): NodeSeq = value(in)
  }

  final class OptionBindParam(val name: String,val value: Option[NodeSeq]) extends Tuple2(name, value) with BindParam {
    def calcValue(in: NodeSeq): NodeSeq = value getOrElse NodeSeq.Empty
  }

  final class BoxBindParam(val name: String,val value: Box[NodeSeq]) extends Tuple2(name, value) with BindParam {
    def calcValue(in: NodeSeq): NodeSeq = value openOr NodeSeq.Empty
  }

  final class SymbolBindParam(val name: String,val value: Symbol) extends Tuple2(name, value) with BindParam {
    def calcValue(in: NodeSeq): NodeSeq = Text(value.name)
  }

  final class IntBindParam(val name: String,val value: Int) extends Tuple2[String, Int](name, value) with BindParam {
    def calcValue(in: NodeSeq): NodeSeq = Text(value.toString)
  }

  final class LongBindParam(val name: String,val value: Long) extends Tuple2[String, Long](name, value) with BindParam {
    def calcValue(in: NodeSeq): NodeSeq = Text(value.toString)
  }

  final class BooleanBindParam(val name: String,val value: Boolean) extends Tuple2[String, Boolean](name, value) with BindParam {
    def calcValue(in: NodeSeq): NodeSeq = Text(value.toString)
  }

  final class TheBindableBindParam[T <: Bindable](val name: String,val value: T) extends Tuple2[String, T](name, value) with BindParam {
    def calcValue(in: NodeSeq): NodeSeq = value.asHtml
  }

  /**
   * transforms a Box into a Text node
   */
  object BindParamAssoc {
    implicit def canStrBoxNodeSeq(in: Box[Any]): Box[NodeSeq] = in.map(_ match {
        case null => Text("null")
        case v => Text(v.toString)
      })
  }

  private def snToNs(in: Seq[Node]): NodeSeq = in

  class SuperArrowAssoc(name: String) {
    // Because JsObj is a subclass of Node, we don't want it
    // getting caught because it's not a bind param
    def ->[T <: SpecialNode](in: T with SpecialNode) = Tuple2[String, T](name, in)

    def ->(in: String) = new TheStrBindParam(name, in)
    def ->(in: NodeSeq) = new TheBindParam(name, in)
    def ->(in: Text) = new TheBindParam(name, in)
    def ->(in: Node) = new TheBindParam(name, in)
    def ->(in: Seq[Node]) = new TheBindParam(name, in)
    def ->(in: NodeSeq => NodeSeq) = new FuncBindParam(name, in)
    def ->(in: Box[NodeSeq]) = new BoxBindParam(name, in)
    def ->(in: Option[NodeSeq]) = new OptionBindParam(name, in)
    def ->(in: Symbol) = new SymbolBindParam(name, in)
    def ->(in: Int) = new IntBindParam(name, in)
    def ->(in: Long) = new LongBindParam(name, in)
    def ->(in: Boolean) = new BooleanBindParam(name, in)
    def ->[T <: Bindable](in: T with Bindable) = new TheBindableBindParam[T](name, in)
    def ->[T](in: T) = Tuple2[String, T](name, in)

    def -%>(in: Elem) = new FuncBindParam(name, old => in % (BindHelpers.currentNode.map(_.attributes) openOr Null))
    def -%>(in: Box[Elem]) = new FuncBindParam(name, old => in.map(_ % (BindHelpers.currentNode.map(_.attributes) openOr Null)) openOr NodeSeq.Empty)
    def -%>(in: Option[Elem]) = new FuncBindParam(name, old => in.map(_ % (BindHelpers.currentNode.map(_.attributes) openOr Null)) getOrElse NodeSeq.Empty)
    def -%>(in: NodeSeq => Elem) = new FuncBindParam(name, old => in(old) % (BindHelpers.currentNode.map(_.attributes) openOr Null))


    def _id_>(in: Elem) = new FuncBindParam(name, _ => in % new UnprefixedAttribute("id", name, Null))
    def _id_>(in: Box[Elem]) = new FuncBindParam(name, _ => in.map(_ % new UnprefixedAttribute("id", name, Null)) openOr NodeSeq.Empty)
    def _id_>(in: Option[Elem]) = new FuncBindParam(name, _ => in.map(_ % new UnprefixedAttribute("id", name, Null)) getOrElse NodeSeq.Empty)
    def _id_>(in: NodeSeq => Elem) = new FuncBindParam(name, kids => in(kids) % new UnprefixedAttribute("id", name, Null))

  }

  implicit def strToSuperArrowAssoc(in: String): SuperArrowAssoc = new SuperArrowAssoc(in)


  /**
   * This class creates a BindParam from an input value
   *
   * @deprecated use -> instead
   */
  @deprecated("use -> instead")
  class BindParamAssoc(val name: String) {
    def -->(value: String): BindParam = new TheBindParam(name, Text(value))
    def -->(value: NodeSeq): BindParam = new TheBindParam(name, value)
    def -->(value: Symbol): BindParam = new TheBindParam(name, Text(value.name))
    def -->(value: Any): BindParam = new TheBindParam(name, Text(if (value == null) "null" else value.toString))
    def -->(func: NodeSeq => NodeSeq): BindParam = new FuncBindParam(name, func)
    def -->(value: Box[NodeSeq]): BindParam = new TheBindParam(name, value.openOr(Text("Empty")))
  }

  /**
   * transforms a String to a BindParamAssoc object which can be associated to a BindParam object
   * using the --> operator.<p/>
   * Usage: <code>"David" --> "name"</code>
   *
   * @deprecated use -> instead
   */
  @deprecated("use -> instead")
  implicit def strToBPAssoc(in: String): BindParamAssoc = new BindParamAssoc(in)

  /**
   * transforms a Symbol to a BindParamAssoc object which can be associated to a BindParam object
   * using the --> operator.<p/>
   * Usage: <code>'David --> "name"</code>
   *
   * @deprecated use -> instead
   */
  @deprecated("use -> instead")
  implicit def symToBPAssoc(in: Symbol): BindParamAssoc = new BindParamAssoc(in.name)

  /**
   * Experimental extension to bind which passes in an additional "parameter" from the XHTML to the transform
   * function, which can be used to format the returned NodeSeq.
   *
   * @deprecated use bind instead
   */
  @deprecated("use bind instead")
  def xbind(namespace: String, xml: NodeSeq)(transform: PartialFunction[String, NodeSeq => NodeSeq]): NodeSeq = {
    def rec_xbind(xml: NodeSeq): NodeSeq = {
      xml.flatMap {
        node => node match {
          case s: Elem if (node.prefix == namespace) =>
            if (transform.isDefinedAt(node.label))
            transform(node.label)(node)
            else
            Text("FIX"+"ME failed to bind <"+namespace+":"+node.label+" />")
          case Group(nodes) => Group(rec_xbind(nodes))
          case s: Elem => Elem(node.prefix, node.label, node.attributes, node.scope, rec_xbind(node.child) : _*)
          case n => node
        }
      }
    }

    rec_xbind(xml)
  }

  /**
   * Bind a set of values to parameters and attributes in a block of XML.<p/>
   *
   * For example: <pre>
   *   bind("user", <user:hello>replace this</user:hello>, "hello" -> <h1/>)
   * </pre>
   * will return <pre><h1></h1></pre>

   * @param namespace the namespace of tags to bind
   * @param xml the NodeSeq in which to find elements to be bound.
   * @param params the list of BindParam bindings to be applied
   *
   * @return the NodeSeq that results from the specified transforms
   */
  def bind(namespace: String, xml: NodeSeq, params: BindParam*): NodeSeq =
  bind(namespace, Empty, Empty , xml, params :_*)

  /**
   * Bind a set of values to parameters and attributes in a block of XML
   * with defined transforms for unbound elements within the specified
   * namespace.<p/>
   *
   * For example:<pre>
   *   bind("user",
   *        Full(xhtml: NodeSeq => Text("Default Value")),
   *        Empty,
   *        <user:hello>replace this</user:hello><user:dflt>replace with default</user:dflt>,
   *        "hello" -> <h1/>)
   * </pre>
   * will return <pre><h1></h1>Default Value</pre>
   *
   * @param namespace the namespace of tags to bind
   * @param nodeFailureXform a box containing the function to use as the default transform
   *        for tags in the specified namespace that do not have bindings specified.
   * @param paramFailureXform a box containing the function to use as the default transform
   *        for unrecognized attributes in bound elements.
   * @param xml the NodeSeq in which to find elements to be bound.
   * @param params the list of BindParam bindings to be applied
   *
   * @return the NodeSeq that results from the specified transforms
   */
  def bind(namespace: String, nodeFailureXform: Box[NodeSeq => NodeSeq],
           paramFailureXform: Box[PrefixedAttribute => MetaData],
           xml: NodeSeq, params: BindParam*): NodeSeq = {
    BindHelpers._bindNodes.doWith(xml :: (BindHelpers._bindNodes.box.openOr(Nil))) {
      val map: _root_.scala.collection.immutable.Map[String, BindParam] = _root_.scala.collection.immutable.HashMap.empty ++ params.map(p => (p.name, p))

      def attrBind(attr: MetaData): MetaData = attr match {
        case Null => Null
        case upa: UnprefixedAttribute => new UnprefixedAttribute(upa.key, upa.value, attrBind(upa.next))
        case pa: PrefixedAttribute if pa.pre == namespace => map.get(pa.key) match {
            case None => paramFailureXform.map(_(pa)) openOr new PrefixedAttribute(pa.pre, pa.key, Text("FIX"+"ME find to bind attribute"), attrBind(pa.next))
            case Some(abp: BindWithAttr) => new UnprefixedAttribute(abp.newAttr, abp.calcValue(pa.value), attrBind(pa.next))
            case Some(bp: BindParam) => new PrefixedAttribute(pa.pre, pa.key, bp.calcValue(pa.value), attrBind(pa.next))
          }
        case pa: PrefixedAttribute => new PrefixedAttribute(pa.pre, pa.key, pa.value, attrBind(pa.next))
      }

      def in_bind(xml: NodeSeq): NodeSeq = {
        xml.flatMap {
          case s : Elem if s.prefix == namespace => BindHelpers._currentNode.doWith(s) {
              map.get(s.label) match {
                case None =>
                  nodeFailureXform.map(_(s)) openOr s

                case Some(ns) =>
                  //val toRet = ns.calcValue(s.child)
                  //mergeBindAttrs(toRet, namespace, s.attributes)
                  ns.calcValue(s.child)
              }
            }
          case s : Elem if bindByNameType(s.label) && bindByNameTag(namespace, s) != "" => BindHelpers._currentNode.doWith(s) {
              val tag = bindByNameTag(namespace, s)
              map.get(tag) match {
                case None => nodeFailureXform.map(_(s)) openOr s
                case Some(bindParam) => bindByNameMixIn(bindParam, s)
              }
            }
          case Group(nodes) => Group(in_bind(nodes))
          case s : Elem => Elem(s.prefix, s.label, attrBind(s.attributes), s.scope, in_bind(s.child) : _*)
          case n => n
        }
      }

      in_bind(xml)
    }
  }

  private def setElemId(in: NodeSeq, attr: String, value: Seq[Node]): NodeSeq =
  in.map {
    case e: Elem => e % new UnprefixedAttribute(attr, value, Null)
    case v => v
  }

  /*
   private def mergeBindAttrs(in: NodeSeq, nameSpace: String, attrs: MetaData): NodeSeq = attrs match {
   case Null => in
   case p: PrefixedAttribute if p.pre == nameSpace =>
   mergeBindAttrs(setElemId(in, p.key, p.value), nameSpace, p.next)
   case m => mergeBindAttrs(in, nameSpace, m.next)
   }
   */

  /**
   * Replace the content of lift:bind nodes with the corresponding nodes found in a map,
   * according to the value of the "name" attribute.<p/>
   * Usage: <pre>
   *   bind(Map("a" -> <h1/>), <b><lift:bind name="a">change this</lift:bind></b>) must ==/(<b><h1></h1></b>)
   * </pre>
   *
   * @param vals map of name/nodes to replace
   * @param xml nodes containing lift:bind nodes
   *
   * @return the NodeSeq that results from the specified transforms
   */
  def bind(vals: Map[String, NodeSeq], xml: NodeSeq): NodeSeq = {

    val isBind = (node: Elem) => {
      node.prefix == "lift" && node.label == "bind"
    }

    xml.flatMap {
      node => node match {
        case s : Elem if (isBind(s)) => {
            node.attributes.get("name") match {
              case None => {
                  if (Props.devMode) {
                    Log.warn("<lift:bind> tag encountered without name attribute!")
                  }
                  bind(vals, node.child)
                }
              case Some(ns) => {
                  def warnOnUnused() =
                  Log.warn("Unused binding values for <lift:bind>: " +
                           vals.keySet.filter(key => key != ns.text).mkString(", "))
                  vals.get(ns.text) match {
                    case None => {
                        if (Props.devMode) {
                          Log.warn("No binding values match the <lift:bind> name attribute: " + ns.text)
                          warnOnUnused()
                        }
                        bind(vals, node.child)
                      }
                    case Some(nodes) => {
                        if (Props.devMode && vals.size > 1) {
                          warnOnUnused()
                        }
                        nodes
                      }
                  }
                }
            }
          }
        case Group(nodes) => Group(bind(vals, nodes))
        case s : Elem => Elem(node.prefix, node.label, node.attributes,node.scope, bind(vals, node.child) : _*)
        case n => node
      }
    }
  }

  /**
   * Bind a list of name/xml maps to a block of XML containing lift:bind nodes (see the bind(Map, NodeSeq) function)
   * @return the NodeSeq that results from the specified transforms
   */
  def bindlist(listvals: List[Map[String, NodeSeq]], xml: NodeSeq): Box[NodeSeq] = {
    def build (listvals: List[Map[String, NodeSeq]], ret: NodeSeq): NodeSeq = listvals match {
      case Nil => ret
      case vals :: rest => build(rest, ret ++ bind(vals, xml))
    }
    if (listvals.length > 0) Full(build(listvals.drop(1), bind(listvals.head, xml)))
    else Empty
  }

  /**
   * Bind parameters to XML.
   *
   * @param around XML with lift:bind elements
   * @param atWhat data to bind
   * @deprecated use the bind function instead
   */
  @deprecated("use the bind function instead")
  def processBind(around: NodeSeq, atWhat: Map[String, NodeSeq]) : NodeSeq = {

    /** Find element matched predicate f(x).isDefined, and return f(x) if found or None otherwise. */
    def findMap[A, B](s: Iterable[A])(f: A => Option[B]): Option[B] =
    s.projection.map(f).find(_.isDefined).getOrElse(None)

    around.flatMap {
      v =>
      v match {
        case Group(nodes) => Group(processBind(nodes, atWhat))
        case Elem("lift", "bind", attr @ _, _, kids @ _*) =>
          findMap(atWhat) {
            case (at, what) if attr("name").text == at => Some(what)
            case _ => None
          }.getOrElse(processBind(v.asInstanceOf[Elem].child, atWhat))

        case e: Elem => {Elem(e.prefix, e.label, e.attributes, e.scope, processBind(e.child, atWhat): _*)}
        case _ => {v}
      }

    }
  }

  /**
   * Finds the named attribute in specified XML element and returns
   * a Full Box containing the value of the attribute if found.
   * Empty otherwise.
   *
   * @return a Full Box containing the value of the attribute if found; Empty otherwise
   */
  def xmlParam(in: NodeSeq, param: String): Box[String] = {
    val tmp = (in \ ("@" + param))
    if (tmp.length == 0) Empty else Full(tmp.text)
  }

  /**
   * Finds and returns the first node in the specified NodeSeq and its children
   * with the same label and prefix as the specified element.
   */
  def findNode(in: Elem, nodes: NodeSeq): Box[Elem] = nodes match {
    case seq if seq.isEmpty => Empty
    case Seq(x: Elem, xs @_*)
      if x.label == in.label && x.prefix == in.prefix => Full(x)
    case Seq(x, xs @_*) => findNode(in, x.child) or findNode(in, xs)
  }

  // get the attribute string or blank string if it doesnt exist
  private def attrStr(elem: Elem, attr: String): String = elem.attributes.get(attr) match {
    case None => ""
    case Some(Nil) => "" // why is a blank string converted to a List
    case Some(x) => x.toString // get string on scala.xml.Text
  }

  // types that can be bindByName
  private def bindByNameType(b: String) = b == "input" || b == "select" || b == "button" || b == "a"

  // allow bind by name eg - <input name="namespace:tag"/>
  private def bindByNameTag(namespace: String, elem: Elem) = attrStr(elem, "name").replaceAll(namespace+":","")

  // mixin what comes from xhtml with what is programatically added
  private def bindByNameMixIn(bindParam: BindParam, s: Elem): NodeSeq = {
    def mix(nodeSeq: NodeSeq): NodeSeq = nodeSeq match {
      case elem: Elem =>
        // mix in undefined attributes
        val attributes = s.attributes.filter(attr => !elem.attribute(attr.key).isDefined )
        elem % attributes
      case Seq(x1: Elem, x2: Elem) if attrStr(x2, "type") == "checkbox" =>
        x1 ++ mix(x2)

      case other =>
        other
    }
    mix(bindParam.calcValue(s))

  }
}

// vim: set ts=2 sw=2 et:
