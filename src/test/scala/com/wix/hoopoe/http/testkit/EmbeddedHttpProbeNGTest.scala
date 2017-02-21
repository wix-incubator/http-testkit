package com.wix.hoopoe.http.testkit

import akka.actor.ActorSystem
import org.specs2.mutable.{BeforeAfter, SpecificationWithJUnit}
import org.specs2.specification.Scope
import spray.client.pipelining.{Get, Post, sendReceive}
import spray.http._

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import UriMatcher._

class EmbeddedHttpProbeNGTest extends SpecificationWithJUnit {

  "probe using builder api" should {
    "answer with registered path listener" in new ctx {
      val request = HttpRequestMatcher(HttpMethods.GET).withUri(haveSomePath)
      addListener(given = request, thenRespond = notFoundResponse)

      get(somePath) must beNotFound
      get("/some1") must beSuccessful
    }

    "answer with multiple registered listeners" in new ctx {
      val request1 = HttpRequestMatcher(HttpMethods.GET).withUri(havePath(Uri.Path("/some1")))
      val response1 = notFoundResponse
      val request2 = HttpRequestMatcher(HttpMethods.GET).withUri(havePath(Uri.Path("/some2")))
      val response2 = HttpResponse(status = StatusCodes.BadGateway)
      addListener(given = request1, thenRespond = response1)
      addListener(given = request2, thenRespond = response2)

      get("/some1") must beNotFound
      get("/some2") must haveStatus(StatusCodes.BadGateway)
      get("/some3") must beSuccessful
    }

    "answer with header listener" in new ctx {
      val header = HttpHeaders.`Accept-Encoding`(Seq(HttpEncodingRange.*))
      val request = HttpRequestMatcher(HttpMethods.GET).withUri(haveSomePath).withHeader(header)
      addListener(given = request, thenRespond = notFoundResponse)

      get(somePath, header = Some(header)) must beNotFound
      get(somePath, header = Some(HttpHeaders.`Content-Type`(ContentTypes.`text/plain`))) must beSuccessful
      get(somePath, header = None) must beSuccessful
    }

    "answer with entity listener" in new ctx {
      private val expectedEntity = HttpEntity("my beautiful http entity")
      val entityMatcher = HttpEntityMatcher.beEqualTo(expectedEntity)
      val request = HttpRequestMatcher(HttpMethods.GET).withUri(haveSomePath).withEntity(entityMatcher)
      addListener(given = request, thenRespond = notFoundResponse)

      get(somePath, entity = Some(expectedEntity)) must beNotFound
      get(somePath, entity = Some(HttpEntity("yada yada yada"))) must beSuccessful
      get(somePath, entity = None) must beSuccessful
    }

    "answer with protocol listener" in new ctx {
      val protocol = HttpProtocols.`HTTP/1.0`
      val request = HttpRequestMatcher(HttpMethods.GET).withUri(haveSomePath).withProtocol(protocol)
      addListener(given = request, thenRespond = notFoundResponse)

      get(somePath, protocol = Some(protocol)) must beNotFound
      get(somePath, protocol = Some(HttpProtocols.`HTTP/1.1`)) must beSuccessful
      get(somePath, protocol = None) must beSuccessful
    }

    "support get with URI" in new ctx {
      val request = HttpRequestMatcher(HttpMethods.GET).withUri(havePath(Uri("/some")))
      addListener(given = request, thenRespond = notFoundResponse)

      get(somePath) must beNotFound
    }

    "support post" in new ctx {
      val request = HttpRequestMatcher(HttpMethods.POST).withUri(haveSomePath)
      addListener(given = request, thenRespond = notFoundResponse)

      post(somePath) must beNotFound
    }

    "support post with URI" in new ctx {
      val request = HttpRequestMatcher(HttpMethods.POST).withUri(havePath(Uri("/some")))
      addListener(given = request, thenRespond = notFoundResponse)

      post(somePath) must beNotFound
    }

  }

  trait ctx extends Scope with BeforeAfter {
    import EmbeddedHttpProbeNGTest._
    val somePath = "/some"
    val haveSomePath = havePath(Uri.Path(somePath))


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

    val notFoundResponse = HttpResponse(status = StatusCodes.NotFound)



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
