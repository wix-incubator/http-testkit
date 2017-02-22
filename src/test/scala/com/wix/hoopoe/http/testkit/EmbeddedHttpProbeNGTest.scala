package com.wix.hoopoe.http.testkit

import com.wix.hoopoe.http.testkit.UriMatcher._
import org.specs2.mutable.SpecificationWithJUnit
import spray.client.pipelining.{Get, Post}
import spray.http._

import scala.concurrent.Await
import scala.concurrent.duration._

class EmbeddedHttpProbeNGTest extends SpecificationWithJUnit {

  "probe using builder api" should {
    "answer with path matcher listener" in new ctx {
      val request = aGetRequest.withUri(haveSomePath)
      addListener(given = request, thenRespond = notFoundResponse)

      get(somePath) must beNotFound
      get("/some1") must beSuccessful
    }

    "answer with path listener" in new ctx {
      val request = aGetRequest.withUri(Uri(somePath))
      addListener(given = request, thenRespond = notFoundResponse)

      get(somePath) must beNotFound
      get("/some1") must beSuccessful
    }

    "answer with multiple registered listeners" in new ctx {
      val request1 = aGetRequest.withUri(havePath(Uri.Path("/some1")))
      val response1 = notFoundResponse
      val request2 = aGetRequest.withUri(havePath(Uri.Path("/some2")))
      val response2 = HttpResponse(status = StatusCodes.BadGateway)
      addListener(given = request1, thenRespond = response1)
      addListener(given = request2, thenRespond = response2)

      get("/some1") must beNotFound
      get("/some2") must haveStatus(StatusCodes.BadGateway)
      get("/some3") must beSuccessful
    }

    "answer with header listener" in new ctx {
      val request = aGetRequest.withUri(haveSomePath).withHeader(expectedHeader)
      addListener(given = request, thenRespond = notFoundResponse)

      get(somePath, header = Some(expectedHeader)) must beNotFound
      get(somePath, header = Some(unexpectedHeader)) must beSuccessful
      get(somePath, header = None) must beSuccessful
    }

    "answer with entity matcher listener" in new ctx {
      val entityMatcher = HttpEntityMatcher.beEqualTo(expectedEntity)
      val request = aGetRequest.withUri(haveSomePath).withEntity(entityMatcher)
      addListener(given = request, thenRespond = notFoundResponse)

      get(somePath, entity = Some(expectedEntity)) must beNotFound
      get(somePath, entity = Some(unexpectedEntity)) must beSuccessful
      get(somePath, entity = None) must beSuccessful
    }

    "answer with entity listener" in new ctx {
      val request = aGetRequest.withUri(haveSomePath).withEntity(expectedEntity)
      addListener(given = request, thenRespond = notFoundResponse)

      get(somePath, entity = Some(expectedEntity)) must beNotFound
      get(somePath, entity = Some(unexpectedEntity)) must beSuccessful
      get(somePath, entity = None) must beSuccessful
    }

    "answer with protocol listener" in new ctx {
      val request = aGetRequest.withUri(haveSomePath).withProtocol(expectedProtocol)
      addListener(given = request, thenRespond = notFoundResponse)

      get(somePath, protocol = Some(expectedProtocol)) must beNotFound
      get(somePath, protocol = Some(unexpectedProtocol)) must beSuccessful
      get(somePath, protocol = None) must beSuccessful
    }

    "support get with URI" in new ctx {
      val request = aGetRequest.withUri(havePath(Uri("/some")))
      addListener(given = request, thenRespond = notFoundResponse)

      get(somePath) must beNotFound
    }

    "support post" in new ctx {
      val request = aPostRequest.withUri(haveSomePath)
      addListener(given = request, thenRespond = notFoundResponse)

      post(somePath) must beNotFound
    }

    "support post with URI" in new ctx {
      val request = aPostRequest.withUri(havePath(Uri("/some")))
      addListener(given = request, thenRespond = notFoundResponse)

      post(somePath) must beNotFound
    }

  }

  trait ctx extends BaseCtx {
    import EmbeddedHttpProbeNGTest._
    val somePath = "/some"
    val haveSomePath = havePath(Uri.Path(somePath))

    val expectedHeader = HttpHeaders.`Accept-Encoding`(Seq(HttpEncodingRange.*))
    val unexpectedHeader = HttpHeaders.`Content-Type`(ContentTypes.`text/plain`)

    val expectedEntity = HttpEntity("my beautiful http entity")
    val unexpectedEntity = HttpEntity("yada yada yada")

    val expectedProtocol = HttpProtocols.`HTTP/1.0`
    val unexpectedProtocol = HttpProtocols.`HTTP/1.1`

    val notFoundResponse = HttpResponse(status = StatusCodes.NotFound)

    def aPostRequest: HttpRequestMatcher = HttpRequestMatcher(HttpMethods.POST)

    def aGetRequest: HttpRequestMatcher = HttpRequestMatcher(HttpMethods.GET)

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
  }
}

object EmbeddedHttpProbeNGTest {

  implicit class RichHttpRequest protected[EmbeddedHttpProbeNGTest](val request: HttpRequest) extends AnyVal {

    def withEntity(entity: Option[HttpEntity]) = request.copy(entity = entity.getOrElse(request.entity))

    def withProtocol(protocol: Option[HttpProtocol]) = request.copy(protocol = protocol.getOrElse(request.protocol))

    def withHeaders(header: Option[HttpHeader]) = request.copy(headers = header.fold(request.headers) { _ => header.toList })
  }
}
