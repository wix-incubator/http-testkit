# http-testkit
[![Build Status](https://travis-ci.org/wix/http-testkit.svg?branch=master)](https://travis-ci.org/wix/http-testkit)

A collection of tools we use at Wix to test our web services – both as clients and servers. We hope to release more tools in the future.

**NOTE:** As this is a work-in-progress, and is being changed as we get feedback from users, some interfaces might change or be removed at later versions.

### Maven

This library is currently only published for Scala 2.11.x:

```xml
<dependency>
    <groupId>com.wix.hoopoe.http</groupId>
    <artifactId>http-testkit_2.11</artifactId>
    <version>0.0.1</version>
</dependency>
```

## EmbeddedHttpProbe

The [EmbeddedHttpProbe](src/main/scala/com/wix/hoopoe/http/testkit/EmbeddedHttpProbe.scala) is a small HTTP server for testing. It's used extensively within Wix for integration testing of external HTTP services. When we integrate with a service that does not supply a testkit, we are bound to write our own simplicators and this can be done easily with this probe.

It is written in Scala on top of [spray](http://spray.io/) and [Akka](http://akka.io/).

#### Example Usage

```scala
// creates a new probe on a random port
val probe = new EmbeddedHttpProbe
probe.doStart()

// the bound port can be accessed after start
val port = probe.actualPort

// add a handler - partial function from HttpRequest to HttpResponse
probe.handlers += ...

// requests sent to the probe can be accessed
probe.requests
```

See more examples in the [tests](src/test/scala/com/wix/hoopoe/http/testkit/EmbeddedHttpProbeTest.scala).

## License

Copyright (c) 2015 Wix.com Ltd. All Rights Reserved. Use of this source code is governed by a BSD-style license that can be found in the [LICENSE](LICENSE.md) file in the root of the source tree.
