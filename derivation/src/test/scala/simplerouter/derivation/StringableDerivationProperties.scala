package simplerouter.derivation

import org.scalacheck.Prop.*
import org.scalacheck.{Arbitrary, Gen, Prop, Properties}
import simplerouter.Stringable
import simplerouter.laws.StringableLaws.roundTrip

object StringableDerivationProperties extends Properties("StringableDerivation") {
  case class Empty()
  case class Wrapper(value: String)
  case class Pair(first: String, second: String)
  case class Nested(pair: Pair, last: Pair)

  sealed trait Target
  object Target {
    case object ByRequest                    extends Target
    case class ToGroup(id: Long)             extends Target
    case class Named(name: String, id: Long) extends Target
    case class Label(text: String)           extends Target
  }

  sealed trait SameNames
  object First  { case object Name extends SameNames }
  object Second { case object Name extends SameNames }

  sealed trait Chain
  object Chain {
    case object End              extends Chain
    case class Link(next: Chain) extends Chain
  }

  case class Range(from: String, to: Option[String])
  case class Optionals(pair: Option[Pair], nested: Option[Option[String]], last: Option[String])
  case class Node(value: String, next: Option[Node])

  sealed trait Filter
  object Filter {
    case class InRange(range: Range)      extends Filter
    case class Limit(limit: Option[Long]) extends Filter
  }

  case class Flag(value: Option[Boolean])
  object Flag {
    implicit val optionBoolean: Stringable[Option[Boolean]] =
      Stringable[Option[Boolean]] {
        case "yes" => Some(Some(true))
        case "no"  => Some(Some(false))
        case "-"   => Some(None)
        case _     => None
      }(_.fold("-")(if (_) "yes" else "no"))
  }

  implicit val emptyStringable: Stringable[Empty]         = StringableDerivation.derived[Empty]
  implicit val wrapperStringable: Stringable[Wrapper]     = StringableDerivation.derived[Wrapper]
  implicit val pairStringable: Stringable[Pair]           = StringableDerivation.derived[Pair]
  implicit val nestedStringable: Stringable[Nested]       = StringableDerivation.derived[Nested]
  implicit val targetStringable: Stringable[Target]       = StringableDerivation.derived[Target]
  implicit val chainStringable: Stringable[Chain]         = StringableDerivation.derived[Chain]
  implicit val rangeStringable: Stringable[Range]         = StringableDerivation.derived[Range]
  implicit val optionalsStringable: Stringable[Optionals] = StringableDerivation.derived[Optionals]
  implicit val nodeStringable: Stringable[Node]           = StringableDerivation.derived[Node]
  implicit val filterStringable: Stringable[Filter]       = StringableDerivation.derived[Filter]
  implicit val flagStringable: Stringable[Flag]           = {
    import Flag.optionBoolean
    StringableDerivation.derived[Flag]
  }

  /** Weighted toward the characters the format treats specially */
  private val text = Gen.listOf(Gen.frequency(1 -> Gen.oneOf(',', '~'), 2 -> Gen.alphaNumChar)).map(_.mkString)

  implicit val arbPair: Arbitrary[Pair]     = Arbitrary(Gen.zip(text, text).map { case (a, b) => Pair(a, b) })
  implicit val arbNested: Arbitrary[Nested] =
    Arbitrary(Gen.zip(arbPair.arbitrary, arbPair.arbitrary).map { case (a, b) => Nested(a, b) })
  implicit val arbTarget: Arbitrary[Target] =
    Arbitrary(
      Gen.oneOf(
        Gen.const(Target.ByRequest),
        Arbitrary.arbitrary[Long].map(Target.ToGroup(_)),
        Gen.zip(text, Arbitrary.arbitrary[Long]).map { case (a, b) => Target.Named(a, b) },
        text.map(Target.Label(_))
      )
    )

  implicit val arbRange: Arbitrary[Range]         =
    Arbitrary(Gen.zip(text, Gen.option(text)).map { case (a, b) => Range(a, b) })
  implicit val arbOptionals: Arbitrary[Optionals] =
    Arbitrary(Gen.zip(Gen.option(arbPair.arbitrary), Gen.option(Gen.option(text)), Gen.option(text)).map {
      case (a, b, c) => Optionals(a, b, c)
    })
  implicit val arbNode: Arbitrary[Node]           = Arbitrary(Gen.nonEmptyListOf(text).map { values =>
    values.init.foldRight(Node(values.last, None))((value, next) => Node(value, Some(next)))
  })
  implicit val arbFilter: Arbitrary[Filter]       =
    Arbitrary(
      Gen.oneOf(
        arbRange.arbitrary.map(Filter.InRange(_)),
        Gen.option(Arbitrary.arbitrary[Long]).map(Filter.Limit(_))
      )
    )

  property("a case class round-trips") = roundTrip(pairStringable)
  property("a case class of case classes round-trips") = roundTrip(nestedStringable)
  property("a sealed trait round-trips") = roundTrip(targetStringable)

  property("a case class with an Option field round-trips") = roundTrip(rangeStringable)
  property("Options of case classes and of Options round-trip") = roundTrip(optionalsStringable)
  property("a case class recursive through an Option round-trips") = roundTrip(nodeStringable)
  property("a sealed trait with Option fields round-trips") = roundTrip(filterStringable)

  property("a case class with no fields is the empty text") =
    (emptyStringable.format(Empty()) ?= "") && (emptyStringable.parse("") ?= Some(Empty()))
  property("a case class with one field is that field, unescaped") =
    (wrapperStringable.format(Wrapper("a,~b")) ?= "a,~b") && (wrapperStringable.parse("a,~b") ?= Some(Wrapper("a,~b")))
  property("all fields but the last are escaped") = nestedStringable.format(Nested(Pair("a", "b,c"), Pair("~", "d"))) ?=
    "a~,b~,c,~~,d"
  property("a case object is its name") = targetStringable.format(Target.ByRequest) ?= "ByRequest"
  property("a subtype with fields is its name, a comma and its text") =
    (targetStringable.format(Target.ToGroup(42)) ?= "ToGroup,42") &&
      (targetStringable.format(Target.Named("a,b", 42)) ?= "Named,a~,b,42")
  property("a subtype whose text is empty is its name") = targetStringable.format(Target.Label("")) ?= "Label"

  property("a text with too few fields doesn't parse") = pairStringable.parse("a") ?= None
  property("a text with an escape the format doesn't produce doesn't parse") = pairStringable.parse("~a,b") ?= None
  property("an unknown subtype doesn't parse") = targetStringable.parse("Other,1") ?= None

  property("subtypes with the same name are rejected") =
    Prop.throws(classOf[IllegalArgumentException])(StringableDerivation.derived[SameNames])

  property("a recursive sealed trait") =
    (chainStringable.format(Chain.Link(Chain.Link(Chain.End))) ?= "Link,Link,End") &&
      (chainStringable.parse("Link,Link,End") ?= Some(Chain.Link(Chain.Link(Chain.End))))

  property("None is the empty text") = (rangeStringable.format(Range("0900", None)) ?= "0900,") &&
    (filterStringable.format(Filter.Limit(None)) ?= "Limit")
  property("Some is the text of its value") = rangeStringable.format(Range("0900", Some("1700"))) ?= "0900,1700"
  property("Some of an empty text or one starting with ~ has a ~ before it") =
    (rangeStringable.format(Range("0900", Some(""))) ?= "0900,~") &&
      (rangeStringable.format(Range("0900", Some("~a"))) ?= "0900,~~a") &&
      (optionalsStringable.format(Optionals(None, Some(None), None)) ?= ",~~,")

  property("a Stringable for an Option field is used if there is one") =
    (flagStringable.format(Flag(None)) ?= "-") && (flagStringable.format(Flag(Some(true))) ?= "yes")
}
