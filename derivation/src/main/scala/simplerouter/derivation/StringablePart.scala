package simplerouter.derivation

import simplerouter.Stringable

/** The `Stringable` that [[StringableDerivation]] uses for a field of a case class or a subtype of a sealed trait
  *
  * There is one for each type that has a `Stringable`, and for `Option[A]` when there is one for `A`. Each refers to
  * the instances it is made of only when first used, so that they can be instances that are still being derived.
  */
final class StringablePart[A] private[derivation] (underlying: => Stringable[A]) {
  lazy val stringable: Stringable[A] = underlying
}

object StringablePart extends LowPriorityStringableParts {
  implicit def fromStringable[A](implicit stringable: => Stringable[A]): StringablePart[A] =
    new StringablePart(stringable)
}

private[derivation] trait LowPriorityStringableParts {

  /** Writes an `Option` the way [[StringableDerivation]] describes
    *
    * Its result type is no more specific than [[StringablePart.fromStringable]]'s, so that `fromStringable` takes
    * priority when there is a `Stringable` for the `Option` type.
    */
  implicit def option[O, A](implicit isOption: O =:= Option[A], part: => StringablePart[A]): StringablePart[O] =
    isOption.flip.substituteCo(new StringablePart(Encoding.option(part.stringable)))
}
