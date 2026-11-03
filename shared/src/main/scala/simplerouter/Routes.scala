package simplerouter

object Routes {

  /** The [[Routes]] that matches no location */
  def empty[A]: Routes[A] = RouteSeq(Vector.empty)

  /** The [[Routes]] that tries each of `routes` in order, as [[Routes.& &]] does */
  def apply[A](routes: Routes[A]*) = RouteSeq(routes)
}

/** Routes [[Location]]s to values of type `A`. A [[Route]] pairs one [[Path]] with a handler; [[&]] combines routes
  * into a [[RouteSeq]].
  */
trait Routes[+A] {

  /** The value for a location. The location is [[Location.normalized normalized]], then tried against each route in
    * order; the first route whose path matches it and whose handler returns a value gives the result. It is not defined
    * for a location no route matches.
    */
  def run: PartialFunction[Location, A]

  /** The value [[run]] gives for `location`, or `None` if it isn't defined for it */
  final def runOption(location: Location): Option[A] = run.lift(location)

  /** These routes, with `f` applied to each value they yield. They match the same locations.
    *
    * @example
    *   {{{("add" :/: Param[Int] >> { _ + 1 }).map("000" + _)}}}
    */
  def map[B](f: A => B): Routes[B]

  /** Routes that try these routes first, then `that` */
  final def &[B >: A](that: Routes[B]) = (this, that) match {
    case (RouteSeq(xs), RouteSeq(ys)) => RouteSeq(xs ++ ys)
    case (RouteSeq(xs), y)            => RouteSeq(xs :+ y)
    case (x, RouteSeq(ys))            => RouteSeq(x +: ys)
    case (x, y)                       => Routes(x, y)
  }
}

/** The elementary [[Routes]]: a [[Path]] and a handler for its value. The route does not match a location the path
  * matches if `handler` returns `None`. The DSL operators `>>`, `>>*` and `>>*?` create one.
  */
case class Route[A, +R](path: Path[A], handler: A => Option[R]) extends Routes[R] {
  override def run: PartialFunction[Location, R] = Function.unlift(path.run(handler))
  override def map[R2](f: R => R2): Route[A, R2] = new Route[A, R2](path, handler.andThen(_.map(f)))
}

/** [[Routes]] that try each of `routes` in order */
case class RouteSeq[+A](routes: Seq[Routes[A]]) extends Routes[A] {
  override def run: PartialFunction[Location, A] = routes
    .foldLeft(PartialFunction.empty[Location, A])(_ orElse _.run)

  override def map[B](f: A => B): RouteSeq[B] = RouteSeq(routes.map(_.map(f)))
}
