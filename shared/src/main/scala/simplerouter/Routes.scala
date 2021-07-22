package simplerouter

object Routes {
  def empty[A]: Routes[A]          = RouteSeq(Vector.empty)
  def apply[A](routes: Routes[A]*) = RouteSeq(routes)
}

/** Runs routes (converts a [[Location]] to a value)
  */
trait Routes[+A] {

  /** Computes the value, if any, for the specified location. The location is parsed by each path until a path is found
    * that can parse it, and then the extracted arguments are passed to the corresponding routing function to compute
    * the value returned.
    */
  def run: PartialFunction[Location, A]

  /** Returns a sitelet whose value (yielded by [[run]] ) is chained through the provided function `f`. That is, the
    * value yielded by the resulting sitelet, by `run` for any given location, is the result of applying `f` with the
    * value yielded by the original sitelet (the left side of `map`) by `run` for that same location.
    *
    * @example
    *   {{{"add" :/: arg[Int] >> { _ + 1 } map ("000" + _)}}}
    */
  def map[B](f: A => B): Routes[B]

  final def &[B >: A](that: Routes[B]) = (this, that) match {
    case (RouteSeq(xs), RouteSeq(ys)) => RouteSeq(xs ++ ys)
    case (RouteSeq(xs), y)            => RouteSeq(xs :+ y)
    case (x, RouteSeq(ys))            => RouteSeq(x +: ys)
    case (x, y)                       => Routes(x, y)
  }
}

/** The elementary [[Routes]]: a pair of a [[Path]] and a handler.
  */
case class Route[A, +R](path: Path[A], handler: A => Option[R]) extends Routes[R] {
  override def run: PartialFunction[Location, R] = Function.unlift(path.run(handler))
  override def map[R2](f: R => R2): Route[A, R2] = new Route[A, R2](path, handler.andThen(_.map(f)))
}

/** A [[Routes]] that wraps a sequence of [[Route]] s
  */
case class RouteSeq[+A](routes: Seq[Routes[A]]) extends Routes[A] {
  override def run: PartialFunction[Location, A] = routes
    .foldLeft(PartialFunction.empty[Location, A])(_ orElse _.run)

  override def map[B](f: A => B) = RouteSeq(routes.map(_.map(f)))
}
