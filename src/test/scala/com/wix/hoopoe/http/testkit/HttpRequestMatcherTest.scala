package com.wix.hoopoe.http.testkit

import org.specs2.mutable.SpecificationWithJUnit
import org.specs2.specification.Scope
import spray.http.HttpHeaders.{Host, Location}
import spray.http._


class HttpRequestMatcherTest extends SpecificationWithJUnit {

  "HttpRequestMatcher" should {
    "match on HttpRequest method" in new ctx {
      val request = anHttpRequest.copy(method = HttpMethods.GET)
      HttpRequestMatcher(method = Some(HttpMethods.POST)).matches(request) must beFalse
    }

    "match on HttpRequest uri" in new ctx {
      val request = anHttpRequest.copy(uri = Uri.Path("/my-path").toString)
      HttpRequestMatcher(uri = Some(Uri.Path("/some-other-path").toString)).matches(request) must beFalse
    }

    "match on HttpRequest one header" in new ctx {
      val request = anHttpRequest.copy(headers = List(Host("localhost", 1010)))
      HttpRequestMatcher(headers = Some(Seq(Host("192.1.1.1", 1010)))).matches(request) must beFalse
    }

    "matches on HttpRequest on more than one header" in new ctx {
      val host = Host("localhost", 1010)
      val request = anHttpRequest.copy(headers = List(host, Location(Uri.Path("far/far/away").toString())))
      HttpRequestMatcher(headers = Some(Seq(host))).matches(request) must beTrue
    }

    "match on HttpRequest httpEntity" in new ctx {
      val request = anHttpRequest.copy(entity = HttpEntity("some entity"))
      HttpRequestMatcher(entity = Some(HttpEntity("another entity"))).matches(request) must beFalse
    }

    "match on HttpRequest protocol" in new ctx {
      val request = anHttpRequest.copy(protocol = HttpProtocols.`HTTP/1.1`)
      HttpRequestMatcher(protocol = Some(HttpProtocols.`HTTP/1.0`)).matches(request) must beFalse
    }

    "match with more than one matcher" in new ctx {
      val method = HttpMethods.GET
      val path = Uri.Path("/my-path")
      val request = anHttpRequest.copy(method = method, uri = path.toString)
      HttpRequestMatcher(method = Some(method), uri = Some(path.toString)).matches(request) must beTrue
    }

    "match any HttpRequest" in new ctx {
      val request = anHttpRequest
      HttpRequestMatcher().matches(request) must beTrue
    }
  }

  trait ctx extends Scope {
    val anHttpRequest =
      HttpRequest(method = HttpMethods.POST, uri = Uri.Path("/some/foo").toString(), headers = List(Host("localhost", 1010)), HttpEntity("some text"), HttpProtocols.`HTTP/1.1`)
  }

}
