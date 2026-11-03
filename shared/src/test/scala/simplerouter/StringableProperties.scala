package simplerouter

import java.time.*

import org.scalacheck.Prop.*
import org.scalacheck.{Arbitrary, Gen, Prop, Properties}

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

  property("oneOf") = {
    // Scalacheck's Arbitrary for Java enums doesn't link on Scala.js
    implicit val arbDayOfWeek: Arbitrary[DayOfWeek] = Arbitrary(Gen.oneOf(DayOfWeek.values().toSeq))
    testStringable(Stringable.oneOf(DayOfWeek.values())(_.name))
  }
  property("oneOf parses only the names") = Stringable.oneOf(List(1, 2))(_.toString).parse("3") ?= None
  property("oneOf rejects values with the same name") =
    Prop(scala.util.Try(Stringable.oneOf(List(1, 11))(_.toString.take(1))).isFailure)

  property("base64Url") = testStringable(Stringable.base64Url.xmap(_.toList)(_.toArray))
  property("base64Url uses only unreserved URL characters") = forAll { (bytes: Array[Byte]) =>
    val s = Stringable.base64Url.format(bytes)
    s.forall(c => c.isLetterOrDigit && c < 128 || c == '-' || c == '_') :| s
  }
  property("base64Url doesn't parse other text") = Stringable.base64Url.parse("a+b/") ?= None

  // Strings made mostly of the characters that need escaping
  private val genEscapable     = Gen.listOf(Gen.stringOf(Gen.oneOf('a', ',', ':', '~')))
  private val separatedStrings = Stringable.separatedList[String]()

  property("separatedList") = {
    implicit val arbEscapable: Arbitrary[List[String]] = Arbitrary(genEscapable)
    testStringable(separatedStrings)
  }
  property("separatedList of Int") = testStringable(Stringable.separatedList[Int]())
  property("separatedList in separatedList") = {
    implicit val inner: Stringable[List[String]]          = Stringable.separatedList[String](':')
    implicit val arbNested: Arbitrary[List[List[String]]] = Arbitrary(Gen.listOf(genEscapable))
    testStringable(Stringable.separatedList[List[String]]())
  }
  property("separatedList escapes") = separatedStrings.format(List("a,b", "c~", "")) ?= "a~,b,c~~,"
  property("separatedList of nothing") =
    (separatedStrings.format(Nil) ?= "") && (separatedStrings.parse("") ?= Some(Nil))
  property("separatedList of one empty element") =
    (separatedStrings.format(List("")) ?= "~") && (separatedStrings.parse("~") ?= Some(List("")))
  property("separatedList doesn't parse an unknown escape") = separatedStrings.parse("a~b") ?= None
  property("separatedList doesn't parse an element that doesn't parse") =
    Stringable.separatedList[Int]().parse("1,x") ?= None
  property("separatedList rejects ~ as a separator") =
    Prop(scala.util.Try(Stringable.separatedList[String]('~')).isFailure)
}
