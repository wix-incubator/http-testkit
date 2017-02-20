package com.wix.hoopoe.http.testkit

import akka.actor.ActorSystem
import org.specs2.matcher.Matcher
import org.specs2.mutable.{BeforeAfter, SpecificationWithJUnit}
import org.specs2.specification.Scope
import spray.can.Http.ConnectionException
import spray.client.pipelining._
import spray.http.Uri._
import spray.http._

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}


class EmbeddedHttpProbeTest extends SpecificationWithJUnit {

  "probe" should {

    "record incoming requests" in new ctx {
      get("/some")
      probe.requests must contain(httpRequestFor("/some"))
    }

    "answer with provided handler" in new ctx {
      probe.handlers += {
        case HttpRequest(HttpMethods.GET, Uri.Path("/some"), _, _, _) => HttpResponse(status = StatusCodes.NotFound)
      }

      get("/some") must beNotFound
    }

    "answer with 200 by default" in new ctx {
      get("/some") must beSuccessful
    }

    "allow setting a default answer" in new ctx {
      override lazy val probe: EmbeddedHttpProbe = new EmbeddedHttpProbe(defaultHandler = EmbeddedHttpProbe.NotFoundHandler)
      get("/some") must beNotFound
    }

    "do not accept connections after shutdown" in new ctx {
      probe.doStop()
      get("/some") must throwA[ConnectionException]
    }

  }

  "probe using builder api" should {
    "answer with registered path listener" in new builderApiCtx {
      val request = RequestBuilder().get(Uri.Path("/some")).build
      val response = notFoundResponse
      val listener = Listener().given(request).thenRespondWith(response)

      probe.addListener(listener)

      get("/some") must beNotFound
      get("/some1") must beSuccessful
    }

    "answer with multiple registered listeners" in new builderApiCtx {
      val request1 = RequestBuilder().get(Uri.Path("/some1")).build
      val response1 = notFoundResponse
      val request2 = RequestBuilder().get(Uri.Path("/some2")).build
      val response2 = ResponseBuilder().withStatus(StatusCodes.BadGateway).build
      probe.addListener(Listener().given(request1).thenRespondWith(response1))
      probe.addListener(Listener().given(request2).thenRespondWith(response2))

      get("/some1") must beNotFound
      get("/some2") must haveStatus(StatusCodes.BadGateway)
      get("/some3") must beSuccessful
    }

    "answer with header listener" in new builderApiCtx {
      val header = HttpHeaders.`Accept-Encoding`(Seq(HttpEncodingRange.*))
      val request = RequestBuilder().get(Uri.Path("/some")).withHeader(header).build

      val listener = Listener().given(request).thenRespondWith(notFoundResponse)

      probe.addListener(listener)

      get("/some", Some(header)) must beNotFound
      get("/some", Some(HttpHeaders.`Content-Type`(ContentTypes.`text/plain`))) must beSuccessful
      get("/some", None) must beSuccessful
    }

    "answer with entity listener" in new builderApiCtx {
      val entity = HttpEntity("my beautiful http entity")
      val request = RequestBuilder()
        .get(Uri.Path("/some"))
        .withEntity(entity)
        .build

      val listener = Listener().given(request).thenRespondWith(notFoundResponse)

      probe.addListener(listener)

      get("/some", entity = Some(entity)) must beNotFound
      get("/some", entity = Some(HttpEntity("yada yada yada"))) must beSuccessful
      get("/some", entity = None) must beSuccessful
    }

    // todo test get with Uri instead of path
    // todo enable the user to verify entity by himself
    // todo implement post support
  }

  trait ctx extends Scope with BeforeAfter {
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

    def get(path: String, header: Option[HttpHeader] = None, entity: Option[HttpEntity] = None) = {
      val request: HttpRequest = Get(s"http://localhost:${probe.actualPort}$path")
        .withHeaders(header.toList)
      val requestWithEntity = entity.map(request.withEntity).getOrElse(request)
      Await.result(pipeline(requestWithEntity), 5.seconds)
    }

  }

  trait builderApiCtx extends ctx {
    val notFoundResponse = ResponseBuilder()
      .withStatus(StatusCodes.NotFound)
      .build
  }

  def httpRequestFor(path: String): Matcher[HttpRequest] = { (_: HttpRequest).uri.path } ^^ ===(Path(path))

  private def haveStatus(status: StatusCode) = be_===(status) ^^ ((_: HttpResponse).status aka "status")
  def beSuccessful = haveStatus(StatusCodes.OK)
  def beNotFound = haveStatus(StatusCodes.NotFound)


}
