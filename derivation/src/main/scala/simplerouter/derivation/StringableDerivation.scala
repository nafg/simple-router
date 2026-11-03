package simplerouter.derivation

/** Derives a `Stringable` for a case class or sealed trait, with `derived`:
  * {{{implicit val target: Stringable[Target] = StringableDerivation.derived[Target]}}}
  *
  * Each field of a case class needs a `Stringable` of its own. Each subtype of a sealed trait uses its own `Stringable`
  * if there is one, and is otherwise derived as well.
  *
  * ==Format==
  *
  * A case class is written as the texts of its fields, in order, separated by `,`. Each field but the last is escaped
  * by putting `~` before each `,` and `~` in it. So a case class with one field is written as that field, and a case
  * object, or a case class with no fields, as the empty text.
  *
  * A sealed trait is written as the simple name of the value's subtype, escaped the same way, followed by `,` and the
  * text of the value as that subtype, unless that text is empty. The subtypes must have distinct simple names.
  *
  * {{{
  * sealed trait Target
  * case object ByRequest                    extends Target // ByRequest
  * case class ToGroup(id: Long)             extends Target // ToGroup,42
  * case class Named(name: String, id: Long) extends Target // Named,a~,b,42 for Named("a,b", 42)
  * }}}
  */
object StringableDerivation extends StringableDerivationMagnolia
