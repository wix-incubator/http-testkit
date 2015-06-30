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

    def get(path: String) = {
      val request: HttpRequest = Get(s"http://localhost:${probe.actualPort}$path")
      Await.result(pipeline(request), 5.seconds)
    }

  }

  def httpRequestFor(path: String): Matcher[HttpRequest] = { (_: HttpRequest).uri.path } ^^ ===(Path(path))

  private def haveStatus(status: StatusCode) = be_===(status) ^^ ((_: HttpResponse).status aka "status")
  def beSuccessful = haveStatus(StatusCodes.OK)
  def beNotFound = haveStatus(StatusCodes.NotFound)


}
