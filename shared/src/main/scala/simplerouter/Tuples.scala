package simplerouter

trait TupleSyntax {
  type ->[+A, +B]  = (A, B)
  type ->:[+A, +B] = (A, B)

  object ->: {
    def unapply[A, B](arg: A ->: B) = Some(arg)
  }

  implicit class Any_->:[A](private val self: A) {
    def ->:[B](b: B) = b -> self
  }
}
object Tuples extends TupleSyntax
