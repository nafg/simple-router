package simplerouter

import simplerouter.Tuples.{->:, Any_->:}

trait Implicits[In[_]] {
  protected type From[A]
  protected def toPath[A](from: From[A]): Path[In[A]]
}
trait ImplicitsForToPath[In[_]] extends Implicits[In] {
  implicit class PathTupleHandlerSyntax[A](self: From[A]) {
    private def path                                = toPath(self)
    def >>*?[R](handler: PartialFunction[In[A], R]) = new Route[In[A], R](path, handler.lift)
    def >>*[R](handler: In[A] => R)                 = new Route[In[A], R](path, a => Some(handler(a)))
  }

  abstract class PathNHandlerSyntax[A](self: From[A]) {
    type F[Z]
    protected def uncurry[Z](f: F[Z]): In[A] => Z
    def >>[Z](handler: F[Z]): Route[In[A], Z] = self >>* (a => uncurry(handler)(a))
  }
  abstract class Path0HandlerSyntaxBase(self: From[Unit]) extends PathNHandlerSyntax(self) {
    override type F[Z] = Z
    override protected def uncurry[Z](f: F[Z]) = _ => f
  }
}

trait ImplicitsForToSegment[In[_]]        extends Implicits[In] {
  implicit class PathSegmentSyntax[A](self: From[A]) {
    def :/:(s: String): Path.Segment.Literal[In[A]]                          = Path.Segment.Literal(s, toPath(self))
    def :/:[B](arg: Param.SegmentParam[B]): Path.Segment.Parameter[B, In[A]] =
      Path.Segment.Parameter(arg, toPath(self))
  }
}
trait ImplicitsForToQueryParameter[In[_]] extends Implicits[In] {
  implicit class PathQueryParameterSyntax[A](self: From[A]) {
    def :&:(s: String): Path.Segment.Literal[In[A]]                            = Path.Segment.Literal(s, toPath(self))
    def :&:[B](arg: Param.SegmentParam[B]): Path.Segment.Parameter[B, In[A]]   =
      Path.Segment.Parameter(arg, toPath(self))
    def :&:[B](p: Param.QueryParam[B]): Path.QueryParameter.Optional[B, In[A]] =
      Path.QueryParameter.Optional(p, toPath(self))
    def :&:[B](p: Param.QueryParamRep[B]): Path.QueryParameter.Repeated[B, In[A]] =
      Path.QueryParameter.Repeated(p, toPath(self))
  }
}

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

trait ImplicitsForTuple2Path[In0[_]] extends ImplicitsForToPath[λ[a => In0[a] ->: Unit]] {
  implicit class ParamHandlerSyntax[Y](self: From[Y]) extends PathNHandlerSyntax(self) {
    override type F[Z] = In0[Y] => Z
    override protected def uncurry[Z](f: F[Z]) = { case y ->: () => f(y) }
  }
}

object syntax
    extends ImplicitsForToPath[λ[a => Unit]]
    with ImplicitsForToSegment[λ[a => Unit]] with TupleSyntax {
  override protected type From[A] = String
  override protected def toPath[A](target: String): Path.Segment.Literal[Unit] = Path.Segment.Literal(target, Path.empty)

  implicit class StringHandlerSyntax(self: String) extends Path0HandlerSyntaxBase(self)

  /** The `PAny` instance
    */
  val ** = Path.Segment.All
}
