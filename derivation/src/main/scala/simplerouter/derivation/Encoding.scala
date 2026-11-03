package simplerouter.derivation

import scala.annotation.tailrec

import simplerouter.Stringable

/** The text format of derived instances, which [[StringableDerivation]] documents */
private[derivation] object Encoding {
  private val separator = ','
  private val escape    = '~'

  private def escaped(text: String) = text.flatMap(c => if (c == separator || c == escape) s"$escape$c" else c.toString)

  /** Reads an escaped text up to the first unescaped separator
    *
    * @return
    *   the unescaped text, and what follows the separator if there is one, or `None` if `text` contains an escape that
    *   [[escaped]] doesn't produce
    */
  private def readEscaped(text: String): Option[(String, Option[String])] = {
    val result                                         = new StringBuilder
    @tailrec
    def loop(i: Int): Option[(String, Option[String])] =
      if (i == text.length) Some((result.toString, None))
      else
        text.charAt(i) match {
          case `separator` => Some((result.toString, Some(text.substring(i + 1))))
          case `escape`    =>
            if (i + 1 < text.length && (text.charAt(i + 1) == separator || text.charAt(i + 1) == escape)) {
              result.append(text.charAt(i + 1))
              loop(i + 2)
            } else None
          case c           =>
            result.append(c)
            loop(i + 1)
        }
    loop(0)
  }

  private def joinFields(fields: Seq[String]) =
    if (fields.isEmpty) "" else (fields.init.map(escaped) :+ fields.last).mkString(separator.toString)

  private def splitFields(text: String, count: Int): Option[List[String]] =
    count match {
      case 0 => if (text.isEmpty) Some(Nil) else None
      case 1 => Some(List(text))
      case _ =>
        readEscaped(text).flatMap {
          case (field, Some(rest)) => splitFields(rest, count - 1).map(field :: _)
          case (_, None)           => None
        }
    }

  /** One field of a case class `T` */
  final case class Field[T](format: T => String, parse: String => Option[Any])

  /** @param construct
    *   builds a `T` from the values of `fields`, in order
    */
  def product[T](fields: Seq[Field[T]])(construct: Seq[Any] => T): Stringable[T] =
    Stringable[T] { text =>
      splitFields(text, fields.length).flatMap { texts =>
        val values = fields.lazyZip(texts).map(_.parse(_))
        if (values.forall(_.isDefined)) Some(construct(values.map(_.get))) else None
      }
    }(value => joinFields(fields.map(_.format(value))))

  /** One subtype of a sealed trait `T` */
  final case class Subtype[T](name: String, parse: String => Option[T])

  /** @param describe
    *   the name of the subtype a value belongs to, and the text of the value as that subtype
    * @throws java.lang.IllegalArgumentException
    *   if two subtypes have the same name
    */
  def sum[T](subtypes: Seq[Subtype[T]])(describe: T => (String, String)): Stringable[T] = {
    val duplicateNames = subtypes.groupBy(_.name).collect { case (name, ss) if ss.size > 1 => name }
    require(
      duplicateNames.isEmpty,
      s"Subtypes must have distinct names, but these repeat: ${duplicateNames.mkString(", ")}"
    )
    val byName         = subtypes.map(s => s.name -> s).toMap
    Stringable[T] { text =>
      readEscaped(text).flatMap { case (name, rest) =>
        byName.get(name).flatMap(_.parse(rest.getOrElse("")))
      }
    } { value =>
      val (name, text) = describe(value)
      if (text.isEmpty) escaped(name) else escaped(name) + separator + text
    }
  }
}
