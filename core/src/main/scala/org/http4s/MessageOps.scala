package org.http4s

import org.http4s.Header.{`Set-Cookie`, `Content-Type`}

import scalaz.concurrent.Task

trait MessageOps extends Any {
  type Self

  def mapHeaders(f: Headers => Headers): Self

  def mapAttributes(f: AttributeMap => AttributeMap): Self

  /** Remove headers that satisfy the predicate
    *
    * @param f predicate
    * @return a new message object which lacks the specified headers
    */
  final def filterHeaders(f: Header => Boolean): Self = mapHeaders(_.filter(f))

  final def withAttributes(attributeMap: AttributeMap): Self = mapAttributes(_ => attributeMap)

  /** Generates a new message object with the specified key/value pair appended to the [[org.http4s.AttributeMap]]
    *
    * @param key [[AttributeKey]] with which to associate the value
    * @param value value associated with the key
    * @tparam A type of the value to store
    * @return a new message object with the key/value pair appended
    */
  final def withAttribute[A](key: AttributeKey[A], value: A): Self = mapAttributes(_.put(key, value))

  /** Generates a new message object with the specified key/value pair appended to the [[org.http4s.AttributeMap]]
    *
    * @param entry [[AttributeEntry]] entry to add
    * @tparam V type of the value to store
    * @return a new message object with the key/value pair appended
    */
  final def withAttribute[V](entry: AttributeEntry[V]): Self = withAttribute(entry.key, entry.value)

  /** Added the [[`Content-Type`]] header to the response */
  final def withType(t: MediaType): Self = putHeaders(`Content-Type`(t))

  final def withContentType(contentType: Option[`Content-Type`]): Self = contentType match {
    case Some(t) => putHeaders(t)
    case None => filterHeaders(_.is(`Content-Type`))
  }

  final def removeHeader(key: HeaderKey): Self = filterHeaders(_ isNot key)

  /** Replaces the [[Header]]s of the incoming Request object
    *
    * @param headers [[Headers]] containing the desired headers
    * @return a new Request object
    */
  final def withHeaders(headers: Headers): Self = mapHeaders(_ => headers)

  /** Replace the existing headers with those provided */
  final def withHeaders(headers: Header*): Self = withHeaders(Headers(headers.toList))

  /** Add the provided headers to the existing headers, replacing those of the same header name
    * The passed headers are assumed to contain no duplicate Singleton headers.
    */
  final def putHeaders(headers: Header*): Self = mapHeaders(_.put(headers: _*))

  final def withTrailerHeaders(trailerHeaders: Task[Headers]): Self =
    withAttribute(Message.Keys.TrailerHeaders, trailerHeaders)
}

trait ResponseOps extends Any with MessageOps {

  /** Change the status of this response object
    *
    * @param status value to replace on the response object
    * @tparam S type that can be converted to a [[Status]]
    * @return a new response object with the new status code
    */
  def withStatus[S <% Status](status: S): Self

  /** Add a Set-Cookie header for the provided [[Cookie]] */
  final def addCookie(cookie: Cookie): Self = putHeaders(Header.`Set-Cookie`(cookie))

  /** Add a Set-Cookie header with the provided values */
  final def addCookie(name: String,
                content: String,
                expires: Option[DateTime] = None): Self = addCookie(Cookie(name, content, expires))

  /** Add a [[`Set-Cookie`]] which will remove the specified cookie from the client */
  final def removeCookie(cookie: Cookie): Self = putHeaders(`Set-Cookie`(cookie.copy(content = "",
    expires = Some(DateTime.UnixEpoch), maxAge = Some(0))))

  /** Add a Set-Cookie which will remove the specified cookie from the client */
  final def removeCookie(name: String): Self = putHeaders(Header.`Set-Cookie`(
    Cookie(name, "", expires = Some(DateTime.UnixEpoch), maxAge = Some(0))
  ))
}

