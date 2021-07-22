package simplerouter

import java.time.Instant
import java.util.UUID

import scala.util.Try

/** A typeclass for converting values to and from strings
  */
trait Stringable[A] { outer =>
  def format: A => String
  def parse: String => Option[A]
  def unapply(s: String) = parse(s)

  def xmap[B](f: A => B)(g: B => A)                = Stringable(outer.parse(_).map(f))(b => outer.format(g(b)))
  def xmapPartial[B](f: A => Option[B])(g: B => A) = Stringable(outer.parse(_).flatMap(f))(b => outer.format(g(b)))
}

object Stringable {
  class Impl[A](override val parse: String => Option[A])(override val format: A => String) extends Stringable[A]
  def apply[A](parse: String => Option[A])(format: A => String): Stringable[A] = new Impl(parse)(format)
  def fromToString[A](parse: String => Option[A])                              = apply(parse)(_.toString)
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
