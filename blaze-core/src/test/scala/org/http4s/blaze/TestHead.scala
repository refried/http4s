package org.http4s.blaze

import org.http4s.blaze.pipeline.HeadStage
import org.http4s.blaze.pipeline.Command.{Disconnected, Disconnect, OutboundCommand, EOF}

import java.nio.ByteBuffer

import scala.concurrent.duration.Duration
import scala.concurrent.{ Promise, Future }


abstract class TestHead(val name: String) extends HeadStage[ByteBuffer] {

  private var acc = Vector[Array[Byte]]()
  private val p = Promise[ByteBuffer]

  var closed = false

  def getBytes(): Array[Byte] = acc.toArray.flatten

  def result = p.future

  override def writeRequest(data: ByteBuffer): Future[Unit] = synchronized {
    if (closed) Future.failed(EOF)
    else {
      val cpy = new Array[Byte](data.remaining())
      data.get(cpy)
      acc :+= cpy
      Future.successful(())
    }
  }

  override def stageShutdown(): Unit = synchronized {
    closed = true
    super.stageShutdown()
    p.trySuccess(ByteBuffer.wrap(getBytes()))
  }
}

class SeqTestHead(body: Seq[ByteBuffer]) extends TestHead("SeqTestHead") {
  private val bodyIt = body.iterator

  override def readRequest(size: Int): Future[ByteBuffer] = synchronized {
    if (!closed && bodyIt.hasNext) Future.successful(bodyIt.next())
    else {
      sendInboundCommand(Disconnected)
      Future.failed(EOF)
    }
  }
}

class SlowTestHead(body: Seq[ByteBuffer], pause: Duration) extends TestHead("Slow TestHead") { self =>
  import org.http4s.blaze.util.Execution.scheduler

  private val bodyIt = body.iterator

  private def clear(): Unit = bodyIt.synchronized {
    while(bodyIt.hasNext) bodyIt.next()
  }

  override def outboundCommand(cmd: OutboundCommand): Unit = {
    cmd match {
      case Disconnect => clear()
      case _          => sys.error(s"TestHead received weird command: $cmd")
    }
    super.outboundCommand(cmd)
  }

  override def readRequest(size: Int): Future[ByteBuffer] = {
    val p = Promise[ByteBuffer]

    scheduler.schedule(new Runnable {
      override def run(): Unit = self.synchronized {
        bodyIt.synchronized {
          if (!closed && bodyIt.hasNext) p.trySuccess(bodyIt.next())
          else p.tryFailure(EOF)
        }
      }
    }, pause)

    p.future
  }
}
