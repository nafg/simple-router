package simplerouter.derivation

/** Derives a `Stringable` for a case class or sealed trait.
  *
  * Each field of a case class needs a `Stringable` of its own, except that an `Option` field needs one only for the
  * type it holds. Each subtype of a sealed trait uses its own `Stringable` if there is one, and is otherwise derived as
  * well.
  *
  * ==Format==
  *
  * A case class is written as the texts of its fields, in order, separated by `,`. Each field but the last is escaped
  * by putting `~` before each `,` and `~` in it. So a case class with one field is written as that field, and a case
  * object, or a case class with no fields, as the empty text.
  *
  * An `Option` field is written, for `None`, as the empty text, and for `Some(a)`, as the text of `a`, with `~` put
  * before it if that text is empty or starts with `~`. But a `Stringable` for the field's `Option` type, if an implicit
  * search where `derived` is called finds one, takes priority. So one derived type can have `Option` fields written
  * both ways: those of `Option` types with a `Stringable` of their own, and the rest.
  *
  * A sealed trait is written as the simple name of the value's subtype, escaped the same way, followed by `,` and the
  * text of the value as that subtype, unless that text is empty. The subtypes must have distinct simple names:
  * `derived` throws an `IllegalArgumentException` if they don't.
  *
  * {{{
  * sealed trait Target
  * case object ByRequest                    extends Target // ByRequest
  * case class ToGroup(id: Long)             extends Target // ToGroup,42
  * case class Named(name: String, id: Long) extends Target // Named,a~,b,42 for Named("a,b", 42)
  *
  * case class Range(from: String, to: Option[String]) // 0900, for Range("0900", None)
  * }}}
  *
  * @example
  *   {{{implicit val target: Stringable[Target] = StringableDerivation.derived[Target]}}}
  */
object StringableDerivation extends StringableDerivationMagnolia
