---
title: Scala Scales XML Skip DTD Validation
---

By default, the underlying SAX parser used by the Scales pull parser will validate 
the DTD on load. This can be really slow and requires an internet connection.
You can ignore the DTD spec by creating an instance of `SimpleUnboundedPool`
and telling Scales to use it.

<!--more-->

**Note:** this example is using `Scales 0.6.0-M1` and `Scalaz 7.0.1`.

``` {.scala .numberLines}
import java.io.ByteArrayInputStream
import javax.xml.stream.XMLInputFactory

import scalaz._, Scalaz._
import scalaz.iteratee._, Iteratee.{iterate => _, _}
import scales.utils._, ScalesUtils._
import scales.utils.resources.SimpleUnboundedPool
import scales.xml._, ScalesXml._

object MyFactoryPool extends SimpleUnboundedPool[XMLInputFactory] {
  val affirmative = java.lang.Boolean.TRUE
  val negative = java.lang.Boolean.FALSE

  val supportDTD = "javax.xml.stream.supportDTD"

  def create = XMLInputFactory.newInstance <| { f =>
    f.setProperty(supportDTD, negative)
  }
}

val xml = """<?xml version="1.0"?>
  <!DOCTYPE ONIXMessage SYSTEM "http://www.editeur.org/onix/2.1/reference/onix-international.dtd">
  <ONIXMessage>
    <Header></Header>
    <Product></Product>
  </ONIXMessage>"""

val pull = pullXml(new ByteArrayInputStream(xml.getBytes), parserFactoryPool = MyFactoryPool)
val iter = iterate(List("ONIXMessage"l, "Product"l), pull)
```

#### References {.references}

* [XMLInputFactory](http://docs.oracle.com/javaee/5/api/javax/xml/stream/XMLInputFactory.html)
* [SimpleUnboundedPool](https://github.com/chris-twiner/scalesXml/blob/release/0.6.0/core/src/main/scala/scales/utils/resources/Resources.scala)
