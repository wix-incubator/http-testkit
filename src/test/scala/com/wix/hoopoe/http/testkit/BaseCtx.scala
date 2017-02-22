package com.wix.hoopoe.http.testkit

import akka.actor.ActorSystem
import org.specs2.matcher.Matchers._
import org.specs2.mutable.BeforeAfter
import spray.client.pipelining._
import spray.http.{HttpRequest, HttpResponse, StatusCode, StatusCodes}

import scala.concurrent.Future

/**
  * Created by tzufitb on 22/02/2017.
  */
trait BaseCtx extends BeforeAfter {
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
  protected val pipeline: HttpRequest => Future[HttpResponse] = sendReceive

  def haveStatus(status: StatusCode) = be_===(status) ^^ ((_: HttpResponse).status aka "status")
  def beSuccessful = haveStatus(StatusCodes.OK)
  def beNotFound = haveStatus(StatusCodes.NotFound)
}
