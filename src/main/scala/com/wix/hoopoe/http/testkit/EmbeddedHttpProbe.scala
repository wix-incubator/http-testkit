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

  // Builder for HttpRequest with matchers
  // Builder for HttpResponse with matchers
  // make handlers private
  // encapsulate PartialFunction creation

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

case class RequestBuilder(requestMatcher: HttpRequestMatcher = HttpRequestMatcher()) {


  def post(uri: Uri) = ??? //httpRequest.copy(method = HttpMethods.POST, uri = uri)

  def get(uri: Path): RequestBuilder = copy(requestMatcher = requestMatcher.copy(method = Some(HttpMethods.GET), uri = Some(uri.toString())))

  def withHeader(httpHeader: HttpHeader) = ??? //httpRequest.copy(headers = httpRequest.headers ++ Seq(httpHeader))

  def build: HttpRequestMatcher = requestMatcher


}

case class ResponseBuilder(response: HttpResponse = HttpResponse()) {
  def withStatus(status: StatusCode): ResponseBuilder = copy(response = response.copy(status = status))

  def build: HttpResponse = response
}                                                   

case class Listener(request: HttpRequestMatcher = HttpRequestMatcher(), response: HttpResponse = HttpResponse()) {

  def given(request: HttpRequestMatcher): Listener = copy(request = request)

  def thenRespondWith(response: HttpResponse): Listener = copy(response = response)

}

case class HttpRequestMatcher(method: Option[HttpMethod] = None,
                              uri: Option[Uri] = None,
                              headers: Option[Seq[HttpHeader]] = None,
                              entity: Option[HttpEntity] = None,
                              protocol: Option[HttpProtocol] = None) {

  def matches(request: HttpRequest): Boolean = {
    method.forall(_ == request.method) &&
    uri.forall(_ == request.uri) &&
    headers.forall(_.forall(request.headers contains)) &&
    entity.forall(_ == request.entity) &&
    protocol.forall(_ == request.protocol)
  }
}

