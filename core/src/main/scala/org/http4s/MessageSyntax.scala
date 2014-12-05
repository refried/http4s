package org.http4s

import scalaz.concurrent.Task

object MessageSyntax extends MessageSyntax

trait MessageSyntax {
  implicit def requestSyntax(req: Task[Request]): TaskMessageOps[Request] = new TaskRequestOps(req)

  implicit def responseSyntax(resp: Task[Response]): TaskResponseOps = new TaskResponseOps(resp)
}

trait TaskMessageOps[M <: Message] extends Any with MessageOps[Task] {
  type Self = Task[M#Self]

  def self: Task[M]

  final override def httpVersion: Task[HttpVersion] = self.map(_.httpVersion)

  final override def headers: Task[Headers] = self.map(_.headers)

  final override def body: Task[EntityBody] = self.map(_.body)

  final override def attributes: Task[AttributeMap] = self.map(_.attributes)

  /** Add a body to the message
    * @see [[Message]]
    */
  def withBody[T](b: T)(implicit w: Writable[T]): Self = self.flatMap(_.withBody(b)(w))

  override def mapHeaders(f: (Headers) => Headers): Self = self.map(_.mapHeaders(f))

  override def mapAttributes(f: (AttributeMap) => AttributeMap): Self = self.map(_.mapAttributes(f))
}

final class TaskRequestOps(val self: Task[Request]) extends AnyVal with TaskMessageOps[Request]

final class TaskResponseOps(val self: Task[Response]) extends AnyVal with TaskMessageOps[Response] with ResponseOps[Task] {
  /** Response specific extension methods */
  override def withStatus[S <% Status](status: S): Self = self.map(_.withStatus(status))
}
