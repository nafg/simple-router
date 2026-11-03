package simplerouter

import java.time.Instant

import org.scalacheck.{Arbitrary, Properties}
import simplerouter.laws.StringableLaws.roundTrip

object StringableProperties extends Properties("Stringable") {
  implicit def arbInstant: Arbitrary[Instant] = Arbitrary(Arbitrary.arbitrary[Long].map(Instant.ofEpochMilli))

  property("string") = roundTrip(implicitly[Stringable[String]])
  property("long") = roundTrip(implicitly[Stringable[Long]])
  property("int") = roundTrip(implicitly[Stringable[Int]])
  property("bool") = roundTrip(implicitly[Stringable[Boolean]])
}
