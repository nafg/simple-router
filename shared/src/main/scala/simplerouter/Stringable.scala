package simplerouter

import java.time.Instant
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

  implicit val string: Stringable[String]       = apply(Some(_))(identity)
  implicit val long: Stringable[Long]           = attempt(_.toLong)(_.toString)
  implicit val int: Stringable[Int]             = attempt(_.toInt)(_.toString)
  implicit val bool: Stringable[Boolean]        = fromToString {
    case "true"  => Some(true)
    case "false" => Some(false)
    case _       => None
  }
  implicit val instant: Stringable[Instant]     = attempt(Instant.parse)(_.toString)
  implicit val uuidStringable: Stringable[UUID] = attempt(UUID.fromString)(_.toString)
}
