package simplerouter

import java.time.{Duration, Instant, LocalDate, LocalDateTime, LocalTime, ZonedDateTime}
import java.util.{Base64, UUID}

import scala.annotation.tailrec
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

  /** Bytes as base64url (RFC 4648 section 5), whose characters need no escaping in a URL. Formats without padding, and
    * parses with or without it.
    *
    * @example
    *   {{{implicit val filter: Stringable[Filter] = Stringable.base64Url.xmapPartial(Filter.decode)(Filter.encode)}}}
    */
  val base64Url: Stringable[Array[Byte]] =
    attempt(Base64.getUrlDecoder.decode(_: String))(Base64.getUrlEncoder.withoutPadding.encodeToString)

  private final val ListEscape = '~'

  /** A list as one text: its elements as `A` formats them, separated by `separator`. Within an element, `~` and
    * `separator` are escaped by a preceding `~`. The empty list is the empty string, and a list of one empty element is
    * `~`. A text parses only if each of its elements does.
    *
    * In a query string, [[Param.repeatable]] can hold a list with no escaping, as one parameter per element.
    *
    * @param separator
    *   the character between elements; it may not be `~`
    * @example
    *   {{{implicit val ids: Stringable[List[Int]] = Stringable.separatedList[Int]()   // e.g. 1,2,3}}}
    * @throws java.lang.IllegalArgumentException
    *   if `separator` is `~`
    */
  def separatedList[A](separator: Char = ',')(implicit A: Stringable[A]): Stringable[List[A]] = {
    require(separator != ListEscape, s"The separator may not be $ListEscape")

    def escape(element: String) =
      element.flatMap(c => if (c == ListEscape || c == separator) s"$ListEscape$c" else c.toString)

    @tailrec
    def split(rest: List[Char], element: StringBuilder, elements: List[String]): Option[List[String]] = rest match {
      case Nil                                                          => Some((element.result() :: elements).reverse)
      case ListEscape :: c :: tail if c == ListEscape || c == separator => split(tail, element += c, elements)
      case ListEscape :: _                                              => None
      case c :: tail if c == separator => split(tail, new StringBuilder, element.result() :: elements)
      case c :: tail                   => split(tail, element += c, elements)
    }

    def parseAll(elements: List[String]) =
      elements.foldRight(Option(List.empty[A]))((element, as) => A.parse(element).flatMap(a => as.map(a :: _)))

    val oneEmptyElement = ListEscape.toString

    apply {
      case ""                => Some(Nil)
      case `oneEmptyElement` => parseAll(List(""))
      case s                 => split(s.toList, new StringBuilder, Nil).flatMap(parseAll)
    } {
      case List(a) if A.format(a).isEmpty => oneEmptyElement
      case as                             => as.map(a => escape(A.format(a))).mkString(separator.toString)
    }
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
