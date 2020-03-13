package simplerouter

import java.time.Instant

import org.scalacheck.{Arbitrary, Prop, Properties}
import Prop._


object StringableProperties extends Properties("Stringable") {
  implicit def arbInstant: Arbitrary[Instant] =
    Arbitrary(Arbitrary.arbitrary[Long].map(Instant.ofEpochMilli))

  def testStringable[A: Arbitrary](s: Stringable[A]) = forAll { a: A =>
    s.parse(s.format(a)).fold(falsified :| s"couldn't parse '${s.format(a)}'") { p => p ?= a }
  }

  property("string") = testStringable(implicitly[Stringable[String]])
  property("long") = testStringable(implicitly[Stringable[Long]])
  property("int") = testStringable(implicitly[Stringable[Int]])
  property("bool") = testStringable(implicitly[Stringable[Boolean]])
}
