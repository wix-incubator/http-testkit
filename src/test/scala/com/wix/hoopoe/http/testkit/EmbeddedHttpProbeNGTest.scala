package com.wix.hoopoe.http.testkit

import akka.actor.ActorSystem
import org.specs2.mutable.{BeforeAfter, SpecificationWithJUnit}
import org.specs2.specification.Scope
import spray.client.pipelining.{Get, Post, sendReceive}
import spray.http._

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

class EmbeddedHttpProbeNGTest extends SpecificationWithJUnit {

  "probe using builder api" should {
    "answer with registered path listener" in new ctx {
      val request = HttpRequestMatcher(HttpMethods.GET).withUri(Uri.Path("/some"))
      addListener(given = request, thenRespond = notFoundResponse)

      get("/some") must beNotFound
      get("/some1") must beSuccessful
    }

    "answer with multiple registered listeners" in new ctx {
      val request1 = HttpRequestMatcher(HttpMethods.GET).withUri(Uri.Path("/some1"))
      val response1 = notFoundResponse
      val request2 = HttpRequestMatcher(HttpMethods.GET).withUri(Uri.Path("/some2"))
      val response2 = ResponseBuilder().withStatus(StatusCodes.BadGateway).build
      addListener(given = request1, thenRespond = response1)
      addListener(given = request2, thenRespond = response2)

      get("/some1") must beNotFound
      get("/some2") must haveStatus(StatusCodes.BadGateway)
      get("/some3") must beSuccessful
    }

    "answer with header listener" in new ctx {
      val header = HttpHeaders.`Accept-Encoding`(Seq(HttpEncodingRange.*))
      //val request = RequestBuilder().get(Uri.Path("/some")).withHeader(header).build
      val request = HttpRequestMatcher(HttpMethods.GET).withUri(Uri.Path("/some")).withHeader(header)
      addListener(given = request, thenRespond = notFoundResponse)

      get("/some", header = Some(header)) must beNotFound
      get("/some", header = Some(HttpHeaders.`Content-Type`(ContentTypes.`text/plain`))) must beSuccessful
      get("/some", header = None) must beSuccessful
    }

    "answer with entity listener" in new ctx {
      val entity = HttpEntity("my beautiful http entity")
      val request = HttpRequestMatcher(HttpMethods.GET).withUri(Uri.Path("/some")).withEntity(entity)
      addListener(given = request, thenRespond = notFoundResponse)

      get("/some", entity = Some(entity)) must beNotFound
      get("/some", entity = Some(HttpEntity("yada yada yada"))) must beSuccessful
      get("/some", entity = None) must beSuccessful
    }

    "answer with protocol listener" in new ctx {
      val protocol = HttpProtocols.`HTTP/1.0`
      val request = HttpRequestMatcher(HttpMethods.GET).withUri(Uri.Path("/some")).withProtocol(protocol)
      addListener(given = request, thenRespond = notFoundResponse)

      get("/some", protocol = Some(protocol)) must beNotFound
      get("/some", protocol = Some(HttpProtocols.`HTTP/1.1`)) must beSuccessful
      get("/some", protocol = None) must beSuccessful
    }

    "support get with URI" in new ctx {
      val request = HttpRequestMatcher(HttpMethods.GET).withUri(Uri("/some"))
      addListener(given = request, thenRespond = notFoundResponse)

      get("/some") must beNotFound
    }

    "support post" in new ctx {
      val request = HttpRequestMatcher(HttpMethods.POST).withUri(Uri.Path("/some"))
      addListener(given = request, thenRespond = notFoundResponse)

      post("/some") must beNotFound
    }

    "support post with URI" in new ctx {
      val request = HttpRequestMatcher(HttpMethods.POST).withUri(Uri("/some"))
      addListener(given = request, thenRespond = notFoundResponse)

      post("/some") must beNotFound
    }

    // todo enable the user to verify entity by himself
  }

  trait ctx extends Scope with BeforeAfter {
    import EmbeddedHttpProbeNGTest._
    lazy val probe = new EmbeddedHttpProbe

    override def before: Unit = {
      probe.doStart()
    }

    override def after: Unit = {
      probe.doStop()
    }

    implicit val system = ActorSystem("client")

    // execution context for futures
    import system.dispatcher
    private val pipeline: HttpRequest => Future[HttpResponse] = sendReceive

    def get(path: String,
            header: Option[HttpHeader] = None,
            entity: Option[HttpEntity] = None,
            protocol: Option[HttpProtocol] = None) = {

      val request: HttpRequest =
        Get(s"http://localhost:${probe.actualPort}$path")
        .withEntity(entity)
        .withProtocol(protocol)
        .withHeaders(header)

      Await.result(pipeline(request), 5.seconds)
    }

    def post(path: String) = {
      val request: HttpRequest = Post(s"http://localhost:${probe.actualPort}$path")

      Await.result(pipeline(request), 5.seconds)
    }

    def addListener(given: HttpRequestMatcher, thenRespond: HttpResponse) =
      probe.addListener(Listener().given(given).thenRespondWith(thenRespond))

    val notFoundResponse = ResponseBuilder()
    .withStatus(StatusCodes.NotFound)
    .build

  }

  def haveStatus(status: StatusCode) = be_===(status) ^^ ((_: HttpResponse).status aka "status")
  def beSuccessful = haveStatus(StatusCodes.OK)
  def beNotFound = haveStatus(StatusCodes.NotFound)

}

object EmbeddedHttpProbeNGTest {

  implicit class RichHttpRequest protected[EmbeddedHttpProbeNGTest](val request: HttpRequest) extends AnyVal {

    def withEntity(entity: Option[HttpEntity]) = request.copy(entity = entity.getOrElse(request.entity))

    def withProtocol(protocol: Option[HttpProtocol]) = request.copy(protocol = protocol.getOrElse(request.protocol))

    def withHeaders(header: Option[HttpHeader]) = request.copy(headers = header.fold(request.headers) { _ => header.toList })
  }
}
