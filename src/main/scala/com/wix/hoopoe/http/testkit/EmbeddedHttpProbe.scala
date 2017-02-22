package com.wix.hoopoe.http.testkit

import akka.actor.Status.Failure
import akka.actor._
import akka.io.IO
import akka.pattern.ask
import akka.util.Timeout
import com.wix.hoopoe.http.testkit.EmbeddedHttpProbe._
import spray.can.Http
import spray.http.Uri.Path
import spray.http._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.concurrent._
import scala.concurrent.duration._
import scala.language.postfixOps

class EmbeddedHttpProbe(port: Int = 0, defaultHandler: Handler = OKHandler) { probe =>


  protected implicit val system = ActorSystem(s"EmbeddedHttpProbe")
  protected implicit val askTimeout = Timeout(5.seconds)

  val requests = new ArrayBuffer[HttpRequest] with mutable.SynchronizedBuffer[HttpRequest]

  @deprecated()
  val handlers = new ArrayBuffer[Handler] with mutable.SynchronizedBuffer[Handler]

  def addListener(listener: Listener): Unit = handlers += {
    case httpRequest: HttpRequest if listener.request.matches(httpRequest) => listener.response
  }

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

case class Listener(request: HttpRequestMatcher = HttpRequestMatcher(HttpMethods.GET), response: HttpResponse = HttpResponse()) {

  def given(request: HttpRequestMatcher): Listener = copy(request = request)

  def thenRespondWith(response: HttpResponse): Listener = copy(response = response)

}

case class HttpRequestMatcher(method: HttpMethod,
                              uri: Option[UriMatcher] = None,
                              headers: Option[Seq[HttpHeader]] = None,
                              entity: Option[HttpEntityMatcher] = None,
                              protocol: Option[HttpProtocol] = None) {

  def matches(request: HttpRequest): Boolean = {
    method == request.method &&
    uri.forall(_.matches(request.uri)) &&
    headers.forall(_.forall(request.headers contains)) &&
    entity.forall(_.matches(request.entity)) &&
    protocol.forall(_ == request.protocol)
  }

  def withHeader(httpHeader: HttpHeader): HttpRequestMatcher = copy(headers = Some(headers.getOrElse(Seq()) ++ Seq(httpHeader)))

  def withEntity(entity: HttpEntityMatcher): HttpRequestMatcher = copy(entity = Some(entity))

  def withEntity(entity: HttpEntity): HttpRequestMatcher = withEntity(HttpEntityMatcher.beEqualTo(entity))

  def withProtocol(protocol: HttpProtocol): HttpRequestMatcher = copy(protocol = Some(protocol))

  def withUri(uri: UriMatcher) = copy(uri = Some(uri))

  def withUri(uri: Uri) = copy(uri = Some(UriMatcher.havePath(uri)))

}

trait HttpEntityMatcher {
  def matches(entity: HttpEntity): Boolean
}

object HttpEntityMatcher {
  def beEqualTo(expectedEntity: HttpEntity): HttpEntityMatcher =
    EqualsHttpEntityMatcher(expectedEntity)

  case class EqualsHttpEntityMatcher private[HttpEntityMatcher](expectedEntity: HttpEntity) extends HttpEntityMatcher {
    override def matches(entity: HttpEntity): Boolean = {
      expectedEntity == entity
    }
  }
}

trait UriMatcher {
  def matches(uri: Uri): Boolean
}

object UriMatcher {
  def havePath(uri: Uri): UriMatcher = UriPathEqualityHttpEntityMatcher(uri.path)
  def havePath(path: Path): UriMatcher = UriPathEqualityHttpEntityMatcher(path)

  case class UriPathEqualityHttpEntityMatcher private[UriMatcher](expectedPath: Path) extends UriMatcher {
    override def matches(uri: Uri): Boolean =
      uri.path == expectedPath
  }
}

