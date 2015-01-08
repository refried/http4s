package org.http4s

import scala.xml.Elem

package object scalaxml extends ElemInstances {
  object implicits {
    implicit val ElemIsXml = MediaTypeOf[Elem](MediaType.`application/xml`)
    implicit val ElemIsHtml = MediaTypeOf[Elem](MediaType.`text/html`)
    implicit val ElemIsXhtml = MediaTypeOf[Elem](MediaType.`application/xhtml+xml`)
  }
}
