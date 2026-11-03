package simplerouter

import java.time.{Duration, Instant, LocalDate, LocalDateTime, LocalTime, ZonedDateTime}
import java.util.UUID

import scala.util.Try

/** A typeclass for converting values to and from strings.
  *
  * `parse` must accept every string `format` produces and give back the original value.
  */
trait Stringable[A] { outer =>

  /** The text representing a value */
  def format: A => String

  /** The value a text represents, or `None` if it represents none */
  def parse: String => Option[A]

  /** [[parse]] as an extractor, so a `Stringable` can be used in a pattern */
  def unapply(s: String) = parse(s)

  /** A `Stringable[B]` that represents a `B` the way this one represents the `A` it corresponds to
    *
    * @param f
    *   converts a parsed `A` to a `B`
    * @param g
    *   converts a `B` to the `A` to format
    * @example
    *   {{{implicit val userId: Stringable[UserId] = Stringable.long.xmap(UserId(_))(_.value)}}}
    */
  def xmap[B](f: A => B)(g: B => A) = Stringable(outer.parse(_).map(f))(b => outer.format(g(b)))

  /** Like [[xmap]], for when not every `A` corresponds to a `B`: a text whose `A` `f` maps to `None` does not parse */
  def xmapPartial[B](f: A => Option[B])(g: B => A) = Stringable(outer.parse(_).flatMap(f))(b => outer.format(g(b)))
}

/** Constructors, and the built-in instances */
object Stringable {
  class Impl[A](override val parse: String => Option[A])(override val format: A => String) extends Stringable[A]

  /** A `Stringable` made of the two functions */
  def apply[A](parse: String => Option[A])(format: A => String): Stringable[A] = new Impl(parse)(format)

  /** A `Stringable` that formats with `toString` */
  def fromToString[A](parse: String => Option[A]) = apply(parse)(_.toString)

  /** A `Stringable` whose `parse` is `None` for a text on which the given `parse` throws a non-fatal exception */
  def attempt[A](parse: String => A)(format: A => String) = apply(s => Try(parse(s)).toOption)(format)

  /** A `Stringable` that formats with `toString`, and whose `parse` is `None` for a text on which the given `parse`
    * throws a non-fatal exception
    */
  def attemptFromToString[A](parse: String => A) = attempt(parse)(_.toString)

  /** A `Stringable` for a type with a fixed set of values, such as an enumeration, that formats a value as its `name`
    * and parses only the names of `values`
    *
    * @example
    *   {{{implicit val dayOfWeek: Stringable[DayOfWeek] = Stringable.oneOf(DayOfWeek.values())(_.name)}}}
    * @throws java.lang.IllegalArgumentException
    *   if two different values have the same name
    */
  def oneOf[A](values: Iterable[A])(name: A => String): Stringable[A] = {
    val valuesByName = values.toSeq.distinct.groupBy(name)
    val sharedNames  = valuesByName.collect { case (n, as) if as.size > 1 => n }
    require(sharedNames.isEmpty, s"More than one value is named ${sharedNames.mkString(", ")}")
    val byName       = valuesByName.map { case (n, as) => n -> as.head }
    apply(byName.get)(name)
  }

  implicit val string: Stringable[String]       = apply(Some(_))(identity)
  implicit val long: Stringable[Long]           = attemptFromToString(_.toLong)
  implicit val int: Stringable[Int]             = attemptFromToString(_.toInt)
  implicit val bool: Stringable[Boolean]        = fromToString {
    case "true"  => Some(true)
    case "false" => Some(false)
    case _       => None
  }
  implicit val instant: Stringable[Instant]     = attemptFromToString(Instant.parse)
  implicit val uuidStringable: Stringable[UUID] = attemptFromToString(UUID.fromString)

  /** `()` is `_` */
  implicit val unit: Stringable[Unit] = apply {
    case "_" => Some(())
    case _   => None
  }(_ => "_")

  implicit val double: Stringable[Double]         = attemptFromToString(_.toDouble)
  implicit val bigDecimal: Stringable[BigDecimal] = attemptFromToString(BigDecimal(_))

  /** ISO-8601, e.g. `PT1H30M` */
  implicit val duration: Stringable[Duration] = attemptFromToString(Duration.parse)

  /** ISO-8601, e.g. `2024-03-05` */
  implicit val localDate: Stringable[LocalDate] = attemptFromToString(LocalDate.parse)

  /** ISO-8601, e.g. `10:15:30` */
  implicit val localTime: Stringable[LocalTime] = attemptFromToString(LocalTime.parse)

  /** ISO-8601, e.g. `2024-03-05T10:15:30` */
  implicit val localDateTime: Stringable[LocalDateTime] = attemptFromToString(LocalDateTime.parse)

  /** ISO-8601 with an offset, followed by the zone ID in brackets when the zone is not an offset, e.g.
    * `2024-03-05T10:15:30+01:00[Europe/Paris]`
    */
  implicit val zonedDateTime: Stringable[ZonedDateTime] = attemptFromToString(ZonedDateTime.parse)
}
