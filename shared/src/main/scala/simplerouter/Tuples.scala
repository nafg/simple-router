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
