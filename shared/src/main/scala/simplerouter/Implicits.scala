package simplerouter

import simplerouter.Tuples.{->:, Any_->:}

/** Base of the traits that add the routing DSL's operators to a type.
  *
  * @tparam In
  *   maps the type parameter of a [[From]] value to the value type of the [[Path]] it becomes
  */
trait Implicits[In[_]] {

  /** The type the operators are added to */
  protected type From[A]

  /** The [[Path]] that `from` stands for */
  protected def toPath[A](from: From[A]): Path[In[A]]
}

/** Adds the operators that pair a path with a handler, making a [[Route]] */
trait ImplicitsForToPath[In[_]] extends Implicits[In] {

  /** Route operators whose handler receives the path's whole value, a tuple such as `Int ->: String ->: Unit` */
  implicit class PathTupleHandlerSyntax[A](self: From[A]) {
    private def path = toPath(self)

    /** A [[Route]] that matches only the locations whose value is in `handler`'s domain. For any other location the
      * route does not match, so a [[Routes]] tries its next route.
      */
    def >>*?[R](handler: PartialFunction[In[A], R]) = new Route[In[A], R](path, handler.lift)

    /** A [[Route]] that passes the path's value to `handler` */
    def >>*[R](handler: In[A] => R) = new Route[In[A], R](path, a => Some(handler(a)))
  }

  /** The `>>` operator, whose handler takes the path's parameters as separate, curried arguments */
  abstract class PathNHandlerSyntax[A](self: From[A]) {

    /** The handler's type: a curried function of the path's parameters, in order, returning `Z` */
    type F[Z]
    protected def uncurry[Z](f: F[Z]): In[A] => Z

    /** A [[Route]] that passes the path's parameters to `handler`
      *
      * @example
      *   {{{"add" :/: Param[Int] :/: Param[Int] >> { a => b => a + b }}}}
      */
    def >>[Z](handler: F[Z]): Route[In[A], Z] = self >>* (a => uncurry(handler)(a))
  }

  /** `>>` for a path without parameters. The handler is the route's value itself, evaluated once, when the route is
    * created.
    */
  abstract class Path0HandlerSyntaxBase(self: From[Unit]) extends PathNHandlerSyntax(self) {
    override type F[Z] = Z
    override protected def uncurry[Z](f: F[Z]) = _ => f
  }
}

/** Adds `:/:`, which prepends a path segment */
trait ImplicitsForToSegment[In[_]] extends Implicits[In] {
  implicit class PathSegmentSyntax[A](self: From[A]) {

    /** Prepends the fixed path segment `s`
      *
      * @throws java.lang.IllegalArgumentException
      *   if `s` is empty. An empty string literal is rejected at compile time instead.
      */
    def :/:(s: String): Path.Segment.Literal[In[A]] = {
      require(s.nonEmpty, "\"\" :/: … is not a path segment: use Path.empty for the root")
      Path.Segment.Literal(s, toPath(self))
    }

    /** Never compiles: it makes `"" :/: …` a compile error. The root is [[Path.empty]]. */
    def :/:(s: "")(implicit noEmptyLiteral: Path.NoEmptyLiteral): Nothing = ???

    /** Prepends a path segment that holds a value, as declared by `arg` */
    def :/:[B](arg: Param.SegmentParam[B]): Path.Segment.Parameter[B, In[A]] =
      Path.Segment.Parameter(arg, toPath(self))
  }
}

/** Adds `:&:`, which joins the path segments to its left to the query parameters to its right. A query parameter can
  * only be prepended with `:&:`, never `:/:`.
  */
trait ImplicitsForToQueryParameter[In[_]] extends Implicits[In] {
  implicit class PathQueryParameterSyntax[A](self: From[A]) {

    /** Prepends the fixed path segment `s`, as `:/:` does
      *
      * @throws java.lang.IllegalArgumentException
      *   if `s` is empty. An empty string literal is rejected at compile time instead.
      */
    def :&:(s: String): Path.Segment.Literal[In[A]] = {
      require(s.nonEmpty, "\"\" :&: … is not a path segment: use Path.empty for the root")
      Path.Segment.Literal(s, toPath(self))
    }

    /** Never compiles: it makes `"" :&: …` a compile error. The root is [[Path.empty]]. */
    def :&:(s: "")(implicit noEmptyLiteral: Path.NoEmptyLiteral): Nothing = ???

    /** Prepends a path segment that holds a value, as `:/:` does */
    def :&:[B](arg: Param.SegmentParam[B]): Path.Segment.Parameter[B, In[A]] =
      Path.Segment.Parameter(arg, toPath(self))

    /** Prepends an optional query parameter */
    def :&:[B](p: Param.QueryParam[B]): Path.QueryParameter.Optional[B, In[A]] =
      Path.QueryParameter.Optional(p, toPath(self))

    /** Prepends a repeatable query parameter */
    def :&:[B](p: Param.QueryParamRep[B]): Path.QueryParameter.Repeated[B, In[A]] =
      Path.QueryParameter.Repeated(p, toPath(self))
  }
}

/** The DSL operators on [[Path]] values. `>>` and `construct` exist for paths with up to five parameters; a path with
  * more can still use `>>*`, `>>*?` and [[Path.build]].
  */
trait PathCompanionImplicits
    extends ImplicitsForToPath[λ[a => a]]
    with ImplicitsForToSegment[λ[a => a]]
    with ImplicitsForToQueryParameter[λ[a => a]] {
  override protected type From[A] = Path[A]
  override protected def toPath[A](from: Path[A]) = from

  implicit class Path0HandlerSyntax(self: Path[Unit])                   extends Path0HandlerSyntaxBase(self)
  implicit class Path1HandlerSyntax[Y](self: Path[Y ->: Unit])          extends PathNHandlerSyntax(self) {
    override type F[Z] = Y => Z
    override protected def uncurry[Z](f: F[Z]) = { case y ->: () => f(y) }
  }
  implicit class Path2HandlerSyntax[Y, X](self: Path[X ->: Y ->: Unit]) extends PathNHandlerSyntax(self) {
    override type F[Z] = X => Y => Z
    override protected def uncurry[Z](f: F[Z]) = { case x ->: y ->: () => f(x)(y) }
  }
  implicit class Path3HandlerSyntax[Y, X, W](self: Path[W ->: X ->: Y ->: Unit])
      extends PathNHandlerSyntax(self) {
    override type F[Z] = W => X => Y => Z
    override protected def uncurry[Z](f: F[Z]) = { case w ->: x ->: y ->: () => f(w)(x)(y) }
  }
  implicit class Path4HandlerSyntax[Y, X, W, V](self: Path[V ->: W ->: X ->: Y ->: Unit])
      extends PathNHandlerSyntax(self) {
    override type F[Z] = V => W => X => Y => Z
    override protected def uncurry[Z](f: F[Z]) = { case v ->: w ->: x ->: y ->: () => f(v)(w)(x)(y) }
  }
  implicit class Path5HandlerSyntax[Y, X, W, V, U](self: Path[U ->: V ->: W ->: X ->: Y ->: Unit])
      extends PathNHandlerSyntax(self) {
    override type F[Z] = U => V => W => X => Y => Z
    override protected def uncurry[Z](f: F[Z]) = { case u ->: v ->: w ->: x ->: y ->: () =>
      f(u)(v)(w)(x)(y)
    }
  }

  /** `construct` builds a [[Location]] from a path's parameters, passed as separate arguments in order. It is
    * [[Path.build]] without the tuple.
    */
  implicit class Path0BuildSyntax(self: Path[Unit])                                              {
    def construct = self.build(())
  }
  implicit class Path1BuildSyntax[Z](self: Path[Z ->: Unit])                                     {
    def construct(a: Z) = self.build(a ->: ())
  }
  implicit class Path2BuildSyntax[Z, Y](self: Path[Y ->: Z ->: Unit])                            {
    def construct(y: Y, z: Z) = self.build(y ->: z ->: ())
  }
  implicit class Path3BuildSyntax[Z, Y, X](self: Path[X ->: Y ->: Z ->: Unit])                   {
    def construct(x: X, y: Y, z: Z) = self.build(x ->: y ->: z ->: ())
  }
  implicit class Path4BuildSyntax[Z, Y, X, W](self: Path[W ->: X ->: Y ->: Z ->: Unit])          {
    def construct(w: W, x: X, y: Y, z: Z) = self.build(w ->: x ->: y ->: z ->: ())
  }
  implicit class Path5BuildSyntax[Z, Y, X, W, V](self: Path[V ->: W ->: X ->: Y ->: Z ->: Unit]) {
    def construct(v: V, w: W, x: X, y: Y, z: Z) = self.build(v ->: w ->: x ->: y ->: z ->: ())
  }
}

/** Adds `>>` to a single [[Param]], which stands for a path holding only that parameter */
trait ImplicitsForTuple2Path[In0[_]] extends ImplicitsForToPath[λ[a => In0[a] ->: Unit]] {
  implicit class ParamHandlerSyntax[Y](self: From[Y]) extends PathNHandlerSyntax(self) {
    override type F[Z] = In0[Y] => Z
    override protected def uncurry[Z](f: F[Z]) = { case y ->: () => f(y) }
  }
}

/** Operators for paths that start or end with a string, and the tuple syntax of [[TupleSyntax]].
  *
  * A right-associative operator such as `:/:` is a method of its right operand, so `"users" :/: Param[Int]` needs no
  * import, while `"users" :/: "new"` and `"users" >> value` do: `import simplerouter.syntax._`.
  */
object syntax
    extends ImplicitsForToPath[λ[a => Unit]]
    with ImplicitsForToSegment[λ[a => Unit]] with TupleSyntax {
  override protected type From[A] = String
  override protected def toPath[A](target: String): Path.Segment.Literal[Unit] =
    Path.Segment.Literal(target, Path.empty)

  implicit class StringHandlerSyntax(self: String) extends Path0HandlerSyntaxBase(self)

  /** [[Path.Segment.All]], which matches all the remaining path segments */
  val ** = Path.Segment.All
}
