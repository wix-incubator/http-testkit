package com.wix.hoopoe.http.testkit

import akka.actor.Status.Failure
import akka.actor._
import akka.io.IO
import akka.pattern.ask
import akka.util.Timeout
import spray.can.Http
import spray.http._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.concurrent._
import scala.concurrent.duration._
import EmbeddedHttpProbe._

class EmbeddedHttpProbe(port: Int=0, defaultHandler: Handler=OKHandler) { probe =>

  protected implicit val system = ActorSystem(s"EmbeddedHttpProbe")
  protected implicit val askTimeout = Timeout(5.seconds)

  val requests = new ArrayBuffer[HttpRequest] with mutable.SynchronizedBuffer[HttpRequest]
  val handlers = new ArrayBuffer[Handler] with mutable.SynchronizedBuffer[Handler]

  var actualPort: Int = _

  private val lifecycle = system.actorOf(Props(new LifecycleActor))

  def doStart(): Unit = {
    actualPort = Await.result((lifecycle ? LifecycleActor.Start).mapTo[Int], Duration.Inf)
  }

  def doStop(): Unit = {
    lifecycle ? LifecycleActor.Stop
  }

  def reset() {
    requests.clear()
    handlers.clear()
  }

  private class ProxyServer extends Actor {

    private def handler: Handler = if (handlers.isEmpty) Map.empty else handlers.reduce { _ orElse _ }

    def receive = {
      case _: Http.Connected => sender ! Http.Register(self)
      case r: HttpRequest =>
        requests += r
        val h = handler
        sender ! {
          if (h.isDefinedAt(r)) h(r) else defaultHandler(r)
        }
    }
  }

  private class LifecycleActor extends Actor {

    private var httpListener: ActorRef = _
    private var probeRef: ActorRef = _

    override def receive: Receive = {

      case LifecycleActor.Start =>
        probeRef = sender()
        val server = system.actorOf(Props(new ProxyServer), name = "server")
        IO(Http) ! Http.Bind(server, interface = "localhost", port = probe.port)

      case Http.Bound(address) =>
        httpListener = sender()
        probeRef ! address.getPort

      case Http.CommandFailed(_: Http.Bind) =>
        val ex = new RuntimeException(s"Failed to start EmbeddedHttpProbe on port $port")
        probeRef ! Failure(ex)

      case LifecycleActor.Stop =>
        httpListener ! Http.Unbind(5.seconds)

      case Http.Unbound =>
        context.stop(self)
    }
  }

  private object LifecycleActor {
    object Start
    object Stop
  }
}


trait EmbeddedHttpProbeConstants {

  type Handler = PartialFunction[HttpRequest, HttpResponse]

  val NotFoundHandler: Handler = {
    case _ => HttpResponse(status = StatusCodes.NotFound)
  }

  val OKHandler: Handler = {
    case _ => HttpResponse()
  }
}

object EmbeddedHttpProbe extends EmbeddedHttpProbeConstants

