package com.wix.hoopoe.http.testkit

import org.specs2.mutable.SpecificationWithJUnit
import org.specs2.specification.Scope
import spray.http.HttpHeaders.{Host, Location}
import spray.http._


class HttpRequestMatcherTest extends SpecificationWithJUnit {

  "HttpRequestMatcher" should {
    "match on HttpRequest method" in new ctx {
      val request = aPostHttpRequest.copy(method = HttpMethods.GET)
      aPostHttpRequestMatcher.matches(request) must beFalse
    }

    "match on HttpRequest uri" in new ctx {
      val request = aGetHttpRequest.copy(uri = Uri.Path("/my-path").toString)
      aGetHttpRequestMatcher.copy(uri = Some(Uri.Path("/some-other-path").toString)).matches(request) must beFalse
    }

    "match on HttpRequest one header" in new ctx {
      val request = aGetHttpRequest.copy(headers = List(Host("localhost", 1010)))
      aGetHttpRequestMatcher.copy(headers = Some(Seq(Host("192.1.1.1", 1010)))).matches(request) must beFalse
    }

    "matches on HttpRequest on more than one header" in new ctx {
      val host = Host("localhost", 1010)
      val request = aGetHttpRequest.copy(headers = List(host, Location(Uri.Path("far/far/away").toString())))
      aGetHttpRequestMatcher.copy(headers = Some(Seq(host))).matches(request) must beTrue
    }

    "match on HttpRequest httpEntity" in new ctx {
      val request = aPostHttpRequest.copy(entity = HttpEntity("some entity"))
      aGetHttpRequestMatcher.copy(entity = Some(HttpEntity("another entity"))).matches(request) must beFalse
    }

    "match on HttpRequest protocol" in new ctx {
      val request = aPostHttpRequest.copy(protocol = HttpProtocols.`HTTP/1.1`)
      aGetHttpRequestMatcher.copy(protocol = Some(HttpProtocols.`HTTP/1.0`)).matches(request) must beFalse
    }

    "match any HttpRequest by method only" in new ctx {
      val request = aGetHttpRequest
      aGetHttpRequestMatcher.matches(request) must beTrue
    }
  }

  trait ctx extends Scope {
    def aPostHttpRequest =
      HttpRequest(method = HttpMethods.POST, uri = Uri.Path("/some/foo").toString(), headers = List(Host("localhost", 1010)), HttpEntity("some text"), HttpProtocols.`HTTP/1.1`)

    def aGetHttpRequest = aPostHttpRequest.copy(method = HttpMethods.GET)

    def aGetHttpRequestMatcher = HttpRequestMatcher(method = HttpMethods.GET)

    def aPostHttpRequestMatcher = HttpRequestMatcher(method = HttpMethods.POST)
  }

}
