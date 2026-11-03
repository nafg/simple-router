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

/** Evidence that the right-nested tuple `C` is `A`, with `B` in place of the `Unit` `A` ends with: e.g. `Int ->: Unit`
  * and `String ->: Unit` make `Int ->: String ->: Unit`.
  */
final class TupleConcat[A, B, C] private (val join: (A, B) => C, val split: C => (A, B))

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
