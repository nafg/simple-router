package simplerouter

/** Syntax for the right-nested tuples that hold a [[Path]]'s value, e.g. `Int ->: String ->: Unit`, which is
  * `(Int, (String, Unit))`. Build one with `1 ->: "a" ->: ()`, and match one with `case n ->: s ->: () =>`.
  */
trait TupleSyntax {

  /** `Tuple2`, as a left-associative infix type */
  type ->[+A, +B] = (A, B)

  /** `Tuple2`, as a right-associative infix type, so `A ->: B ->: Unit` is `(A, (B, Unit))` */
  type ->:[+A, +B] = (A, B)

  /** Matches a `Tuple2` as `head ->: tail` */
  object ->: {
    def unapply[A, B](arg: A ->: B) = Some(arg)
  }

  implicit class Any_->:[A](private val self: A) {

    /** The pair of `b` and this value: `b ->: self` */
    def ->:[B](b: B) = b -> self
  }
}

/** [[TupleSyntax]] as an object, so its members can be imported */
object Tuples extends TupleSyntax

/** Evidence that `C` is the right-nested tuple `A`, which ends with `Unit`, with `B` in place of that `Unit`: e.g.
  * `Int ->: Unit` and `String ->: Unit` make `Int ->: String ->: Unit`, and `Int ->: Unit` and `String` make
  * `Int ->: String`.
  *
  * @param join
  *   the `C` for an `A` and a `B`
  * @param split
  *   the `A` and `B` of a `C`, the inverse of `join`
  */
final class TupleConcat[A, B, C] private (val join: (A, B) => C, val split: C => (A, B)) {

  /** Matches a `C` as its `A` and `B`, as [[split]] gives them */
  def unapply(c: C): Some[(A, B)] = Some(split(c))
}

object TupleConcat {
  implicit def unit[B]: TupleConcat[Unit, B, B] = new TupleConcat((_, b) => b, ((), _))

  implicit def pair[H, T, B, TB](implicit tail: TupleConcat[T, B, TB]): TupleConcat[(H, T), B, (H, TB)] =
    new TupleConcat(
      { case ((h, t), b) => (h, tail.join(t, b)) },
      { case (h, tb) =>
        val (t, b) = tail.split(tb)
        ((h, t), b)
      }
    )
}
