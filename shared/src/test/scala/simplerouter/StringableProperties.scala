package simplerouter

import java.time.*

import org.scalacheck.Prop.*
import org.scalacheck.{Arbitrary, Gen, Properties}

object StringableProperties extends Properties("Stringable") {
  implicit def arbInstant: Arbitrary[Instant] = Arbitrary(Arbitrary.arbitrary[Long].map(Instant.ofEpochMilli))

  implicit def arbDuration: Arbitrary[Duration] =
    Arbitrary(for (s <- Arbitrary.arbitrary[Long]; n <- Gen.choose(0, 999999999)) yield Duration.ofSeconds(s, n))

  implicit def arbLocalDate: Arbitrary[LocalDate] =
    Arbitrary(Gen.choose(LocalDate.MIN.toEpochDay, LocalDate.MAX.toEpochDay).map(LocalDate.ofEpochDay))

  implicit def arbLocalTime: Arbitrary[LocalTime] =
    Arbitrary(Gen.choose(0L, LocalTime.MAX.toNanoOfDay).map(LocalTime.ofNanoOfDay))

  implicit def arbLocalDateTime: Arbitrary[LocalDateTime] =
    Arbitrary(for (d <- arbLocalDate.arbitrary; t <- arbLocalTime.arbitrary) yield LocalDateTime.of(d, t))

  implicit def arbZonedDateTimeWithOffset: Arbitrary[ZonedDateTime] =
    Arbitrary(for {
      dt     <- arbLocalDateTime.arbitrary
      offset <- Gen.choose(ZoneOffset.MIN.getTotalSeconds, ZoneOffset.MAX.getTotalSeconds)
    } yield ZonedDateTime.of(dt, ZoneOffset.ofTotalSeconds(offset)))

  def testStringable[A: Arbitrary](s: Stringable[A]) = forAll { (a: A) =>
    s.parse(s.format(a)).fold(falsified :| s"couldn't parse '${s.format(a)}'")(p => p ?= a)
  }

  property("string") = testStringable(implicitly[Stringable[String]])
  property("long") = testStringable(implicitly[Stringable[Long]])
  property("int") = testStringable(implicitly[Stringable[Int]])
  property("bool") = testStringable(implicitly[Stringable[Boolean]])
  property("unit") = testStringable(implicitly[Stringable[Unit]])
  // Compared boxed, because NaN is not == to itself
  property("double") = {
    implicit val arbBoxedDouble: Arbitrary[java.lang.Double] = Arbitrary(Arbitrary.arbitrary[Double].map(Double.box))
    testStringable(implicitly[Stringable[Double]].xmap(Double.box)(_.doubleValue))
  }
  property("bigDecimal") = testStringable(implicitly[Stringable[BigDecimal]])
  property("duration") = testStringable(implicitly[Stringable[Duration]])
  property("localDate") = testStringable(implicitly[Stringable[LocalDate]])
  property("localTime") = testStringable(implicitly[Stringable[LocalTime]])
  property("localDateTime") = testStringable(implicitly[Stringable[LocalDateTime]])
  property("zonedDateTime with an offset") = testStringable(implicitly[Stringable[ZonedDateTime]])
}
