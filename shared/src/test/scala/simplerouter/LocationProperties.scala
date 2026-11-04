package simplerouter

import org.scalacheck.Prop.*
import org.scalacheck.{Properties, Test}

import LocationGen.prettyLocation

object LocationProperties extends Properties("Location") {
  override def overrideParameters(p: Test.Parameters): Test.Parameters = p.withMinSuccessfulTests(2000)

  property("parsePathAndQuery parses toUrl back to the normalized Location") = forAll(LocationGen.location) { loc =>
    Location.parsePathAndQuery(loc.toUrl) ?= loc.normalized
  }

  property("parsePathAndQueryStrict accepts toUrl") = forAll(LocationGen.location) { loc =>
    Location.parsePathAndQueryStrict(loc.toUrl) ?= Some(loc.normalized)
  }
}
