package org.http4s.util

import org.scalacheck.Prop
import org.specs2.{ScalaCheck, Specification}
import UrlCodingUtils.urlEncode

import scalaz.NonEmptyList
import scalaz.scalacheck.ScalazArbitrary._

class UrlFormCodecSpec extends Specification with ScalaCheck {
  def is = {
    s2"""
    UrlFormCodec.encode should include & in multi-valued attributes ${
      Prop.forAll { (keyChars: NonEmptyList[Char], values: NonEmptyList[String]) =>
        val keyString: String = keyChars.list.mkString
        UrlFormCodec.encode(Map(keyString -> values.list)) ===
          values.map(urlEncode(_)).list
            .mkString(
              urlEncode(keyString) + '=',
              '&' + urlEncode(keyString) + '=',
              ""
            )
      }
    }
    UrlFormCodec.encode shouldn't include & in no-valued attributes ${
      Prop.forAll(
        (key: String) =>
          UrlFormCodec.encode(Map(key -> Nil)) === urlEncode(key)
      )
    }
  """
  }
}