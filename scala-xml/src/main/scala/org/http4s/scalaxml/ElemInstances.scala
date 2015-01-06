package org.http4s
package scalaxml

import java.io.StringReader

import Header.`Content-Type`
import scala.util.control.NonFatal
import scala.xml._
import scalaz.{Tag, @@}
import scalaz.concurrent.Task

trait ElemInstances {
  def elemEncoder(mediaType: MediaType)(implicit charset: Charset = Charset.`UTF-8`): EntityEncoder[Elem] =
    EntityEncoder.stringEncoder(charset)
      .contramap[Elem](elem => elem.buildString(false))
      .withContentType(`Content-Type`(MediaType.`application/xml`))

  implicit def taggedElemEncoder[T <: XmlMediaTypeTag](implicit charset: Charset = Charset.`UTF-8`, tag: T): EntityEncoder[Elem @@ T] =
    elemEncoder(tag.mediaType).contramap(Tag.unwrap)

  /**
   * Handles a message body as XML.
   *
   * TODO Not an ideal implementation.  Would be much better with an asynchronous XML parser, such as Aalto.
   *
   * @param parser the SAX parser to use to parse the XML
   * @return an XML element
   */
  implicit def xml(implicit parser: SAXParser = XML.parser): EntityDecoder[Elem] = {
    import EntityDecoder._
    decodeBy(MediaType.`text/xml`, MediaType.`text/html`, MediaType.`application/xml`){ msg =>
      collectBinary(msg).flatMap[Elem] { arr =>
        val source = new InputSource(new StringReader(new String(arr.toArray, msg.charset.getOrElse(Charset.`US-ASCII`).nioCharset)))
        try DecodeResult.success(Task.now(XML.loadXML(source, parser)))
        catch {
          case e: SAXParseException =>
            val msg = s"${e.getMessage}; Line: ${e.getLineNumber}; Column: ${e.getColumnNumber}"
            DecodeResult.failure(Task.now(ParseFailure("Invalid XML", msg)))
          case NonFatal(e) => DecodeResult(Task.fail(e))
        }
      }
    }
  }

  def xml: EntityDecoder[Elem] = xml()
}
