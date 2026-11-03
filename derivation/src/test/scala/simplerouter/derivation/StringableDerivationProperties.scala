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

  implicit val emptyStringable: Stringable[Empty]     = StringableDerivation.derived[Empty]
  implicit val wrapperStringable: Stringable[Wrapper] = StringableDerivation.derived[Wrapper]
  implicit val pairStringable: Stringable[Pair]       = StringableDerivation.derived[Pair]
  implicit val nestedStringable: Stringable[Nested]   = StringableDerivation.derived[Nested]
  implicit val targetStringable: Stringable[Target]   = StringableDerivation.derived[Target]

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

  property("a case class round-trips") = roundTrip(pairStringable)
  property("a case class of case classes round-trips") = roundTrip(nestedStringable)
  property("a sealed trait round-trips") = roundTrip(targetStringable)

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
}
