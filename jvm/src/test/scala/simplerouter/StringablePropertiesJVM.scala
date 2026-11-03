package simplerouter

import java.time.{Instant, ZoneId, ZonedDateTime}

import scala.jdk.CollectionConverters.*

import org.scalacheck.{Arbitrary, Gen, Properties}

object StringablePropertiesJVM extends Properties("Stringable") {
  import StringableProperties.{arbInstant, arbLocalDateTime, testStringable}

  property("instant") = testStringable(implicitly[Stringable[Instant]])

  // Zone regions need time zone data, which the Scala.js tests don't have
  property("zonedDateTime with a region") = {
    implicit val arbZonedDateTimeWithRegion: Arbitrary[ZonedDateTime] =
      Arbitrary(for {
        dt   <- arbLocalDateTime.arbitrary
        zone <- Gen.oneOf(ZoneId.getAvailableZoneIds.asScala.toSeq.sorted)
      } yield ZonedDateTime.of(dt, ZoneId.of(zone)))
    testStringable(implicitly[Stringable[ZonedDateTime]])
  }
}
