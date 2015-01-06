package org.http4s

import scala.xml.Elem
import scalaz.{Tag, @@}

package object scalaxml extends ElemInstances {
  implicit object Html extends XmlMediaTypeTag {
    val mediaType = MediaType.`text/html`
    def apply(elem: Elem): Elem @@ Html.type = Tag.of[Html.type](elem)
  }

  implicit object Xhtml extends XmlMediaTypeTag {
    val mediaType = MediaType.`application/xhtml+xml`
    def apply(elem: Elem): Elem @@ Xhtml.type = Tag.of[Xhtml.type](elem)
  }

  implicit object Xml extends XmlMediaTypeTag {
    val mediaType = MediaType.`application/xml`
    def apply(elem: Elem): Elem @@ Xml.type = Tag.of[Xml.type](elem)
  }
}
