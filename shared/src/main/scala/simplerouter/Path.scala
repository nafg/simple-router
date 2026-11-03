package simplerouter

import scala.annotation.implicitNotFound

import simplerouter.Tuples.{->, Any_->:}

private class Extractor[-A, +B](f: A => Option[B]) {
  def unapply(a: A): Option[B] = f(a)
}

/** A typesafe, reversible URL template: a chain of path segments followed by query parameters, each either fixed or
  * holding a value. [[build]] turns a value into a [[Location]], and [[parse]] turns a `Location` back into the value.
  *
  * Write paths with the DSL rather than the case classes, e.g. `"users" :/: Param[Int] :&: Param[String]("tab")`.
  *
  * @tparam A
  *   the value this path holds. A path made with the DSL holds its parameters' values as a right-nested tuple ending in
  *   `Unit`, e.g. `Int ->: Option[String] ->: Unit` (see [[TupleSyntax]]); [[xmap]] converts it to another type.
  */
sealed trait Path[A] {
  protected[simplerouter] def encode: A => Location => Location

  /** Matches this path against the start of `location`'s path, as it is (not normalized), and takes the query
    * parameters this path declares from `location`'s query. The result is this path's value and what is left of
    * `location`, or `None` if the start of `location` does not match.
    */
  protected[simplerouter] def parsePrefix: Location => Option[(A, Location)]

  /** The [[Location]] this path describes for `param` */
  final def build(param: A): Location = encode(param)(Location(Nil))

  /** The value `location` holds, if it matches this path. The path is matched against `location`'s
    * [[Location.normalized normalized]] form, and only if it accounts for every one of its path segments; query
    * parameters this path does not declare are ignored.
    */
  final def parse: Location => Option[A] = loc => parseNormalized(loc.normalized)

  /** [[parse]] for a `location` that is already [[Location.normalized normalized]] */
  protected[simplerouter] final def parseNormalized(location: Location): Option[A] =
    parsePrefix(location).collect { case (a, Location(Nil, _)) => a }

  /** A function that [[parse parses]] a [[Location]] and passes its value to `handler`. It returns `None` if the
    * location does not match or `handler` returns `None`.
    */
  final def run[R](handler: A => Option[R]): Location => Option[R] = loc => parse(loc).flatMap(handler)

  /** A `Path[B]` that matches the same locations as this path, and holds a `B` where this path holds the `A` it
    * corresponds to. A `B` that is built parses back to itself only if `f(g(b)) == b`.
    *
    * @param f
    *   converts a parsed `A` to a `B`
    * @param g
    *   converts a `B` to the `A` to build
    * @example
    *   {{{
    * case class Page(number: Option[Int], tags: List[String])
    * val page = (Param[Int]("page") :&: Param.repeatable[String]("tag"))
    *   .xmap { case n ->: tags ->: () => Page(n, tags) }(p => p.number ->: p.tags ->: ())
    *   }}}
    */
  final def xmap[B](f: A => B)(g: B => A): Path[B] = Path.Mapped(this, (a: A) => Some(f(a)), g)

  /** Like [[xmap]], for when not every `A` corresponds to a `B`: a location whose `A` `f` maps to `None` does not
    * match. Building does not consult `f`, and a `B` that is built parses back to itself only if `f(g(b)) == Some(b)`.
    */
  final def xmapPartial[B](f: A => Option[B])(g: B => A): Path[B] = Path.Mapped(this, f, g)

  /** This path followed by `that`: its path segments are this path's, then `that`'s, and it has the query parameters of
    * both. Its value is this path's, with `that`'s value in place of the `Unit` this path's value ends with.
    *
    * A path that ends with `**` takes all the remaining path segments, so if this path ends with it and `that` has path
    * segments, the result matches no location.
    *
    * @example
    *   {{{
    * ("users" :/: Param[Long]) ++ ("posts" :&: Param[Int]("page"))   // Path[Long ->: Option[Int] ->: Unit]
    *   }}}
    */
  final def ++[B, C](that: Path[B])(implicit concat: TupleConcat[A, B, C]): Path[C] =
    Path.Concatenated(this, that, concat)
}

object Path extends PathCompanionImplicits {

  /** The root, `/`: the path with no segments. It matches a [[Location]] whose [[Location.normalized normalized]] path
    * has no segments.
    */
  val root = Segment.Empty

  /** The path with no segments, the same as [[root]] */
  val empty = Segment.Empty

  /** A path consisting of the single segment `literal`. The root is [[root]].
    *
    * @throws java.lang.IllegalArgumentException
    *   if `literal` is empty. An empty string literal is rejected at compile time instead.
    */
  def apply(literal: String): Segment.Literal[Unit] = {
    require(literal.nonEmpty, "Path(\"\") is not a path segment: use Path.root for the root")
    Segment.Literal(literal, empty)
  }

  /** Never compiles: it makes `Path("")` a compile error. The root is [[root]]. */
  def apply(literal: "")(implicit noEmptyLiteral: NoEmptyLiteral): Nothing = ???

  /** Has no instances, so that writing an empty path literal fails to compile with this message */
  @implicitNotFound("\"\" is not a path segment: use Path.root for the root")
  sealed abstract class NoEmptyLiteral

  /** A [[Path]] that starts with a path segment, or ends the chain */
  sealed trait Segment[A] extends Path[A]
  object Segment {

    /** The path with no segments, also available as [[Path.root]]. It matches a [[Location]] with no path segments. A
      * chain ends with it unless it ends with [[All]]; the DSL adds it for you.
      */
    case object Empty extends Path.Segment[Unit] {
      override protected[simplerouter] def encode = _ => identity

      override protected[simplerouter] def parsePrefix: Location => Option[(Unit, Location)] = loc => Some(((), loc))
    }

    /** Matches all the remaining path segments, which its part of the value holds as a `List[String]`. It ends a chain.
      * Also available as `syntax.**`.
      */
    case object All extends Path.Segment[List[String] -> Unit] {
      override protected[simplerouter] def encode = param => _ ++ param._1

      override protected[simplerouter] def parsePrefix: Location => Option[(List[String] -> Unit, Location)] =
        loc => Some((loc.path ->: (), loc.copy(path = Nil)))
    }

    /** The fixed path segment `component`, followed by `next`. It adds nothing to the path's value. */
    case class Literal[A](component: String, next: Path[A]) extends Path.Segment[A] {
      override protected[simplerouter] def encode = param => l => next.encode(param)(l :+ component)

      override protected[simplerouter] def parsePrefix: Location => Option[(A, Location)] = {
        case loc @ Location(`component` :: _, _) => next.parsePrefix(loc.tail)
        case _                                   => None
      }
    }

    /** A path segment holding a value, converted by `param`'s [[Stringable]], followed by `next`. A location whose
      * segment `param` cannot parse does not match.
      */
    case class Parameter[A, B](param: Param.SegmentParam[A], next: Path[B]) extends Path.Segment[A -> B] {
      override protected[simplerouter] def encode = { case a -> b =>
        l => next.encode(b)(l :+ param.stringable.format(a))
      }

      override protected[simplerouter] def parsePrefix: Location => Option[(A -> B, Location)] = {
        case loc @ Location(param.stringable(a) :: _, _) =>
          next.parsePrefix(loc.tail).map { case (b, rest) => (a -> b, rest) }
        case _                                           => None
      }
    }
  }

  /** A [[Path]] that starts with a query parameter */
  sealed trait QueryParameter[A] extends Path[A]
  object QueryParameter {

    /** An optional query parameter holding a value, converted by `param`'s [[Stringable]], followed by `next`. Its part
      * of the value is `None` when the location has no query parameter named `param.key`. Otherwise it is the first
      * occurrence's value, and if `param` cannot parse that, the location does not match. Later occurrences are
      * ignored.
      */
    case class Optional[A, B](param: Param.QueryParam[A], next: Path[B]) extends QueryParameter[Option[A] -> B] {
      private val locParam = new Extractor((_: Location).takeParam(param.key))

      override protected[simplerouter] def encode = { case ao -> b =>
        l =>
          val loc2 = ao match {
            case None    => l
            case Some(a) => l & ((param.key, param.stringable format a))
          }
          next.encode(b)(loc2)
      }

      override protected[simplerouter] def parsePrefix: Location => Option[(Option[A] -> B, Location)] = {
        case locParam(param.stringable(a), loc2) => next.parsePrefix(loc2).map { case (b, rest) =>
            (Some(a) -> b, rest)
          }
        case loc                                 => next.parsePrefix(loc).flatMap {
            case (b, rest) if loc.query.forall(_._1 != param.key) => Some((None -> b, rest))
            case _                                                => None
          }
      }
    }

    /** A query parameter that may occur any number of times, followed by `next`. Its part of the value is the list of
      * the values of every occurrence of `params.key`, in order, each converted by `params`'s [[Stringable]]. An
      * occurrence that `params` cannot parse is left out of the list, or, if `params` is
      * [[Param.QueryParamRep.strict strict]], stops the location from matching.
      */
    case class Repeated[A, B](params: Param.QueryParamRep[A], next: Path[B]) extends QueryParameter[List[A] -> B] {
      private val locParams = new Extractor((loc: Location) => Some(loc.takeParams(params.key)))
      private val parseAll  = new Extractor(params.parseAll)

      override protected[simplerouter] def encode = { case as -> b =>
        loc =>
          val location = loc && ((params.key, as.map(params.stringable.format)))
          next.encode(b)(location)
      }

      override protected[simplerouter] def parsePrefix: Location => Option[(List[A] -> B, Location)] = {
        case locParams(parseAll(as), loc2) => next.parsePrefix(loc2).map { case (b, rest) => (as -> b, rest) }
        case _                             => None
      }
    }
  }

  /** Matches the locations `path` matches, holding the `B` that `f` gives for `path`'s value. A location for whose
    * value `f` gives `None` does not match. [[Path.xmap]] and [[Path.xmapPartial]] create one.
    *
    * @param g
    *   gives the value of `path` to build for a `B`
    */
  case class Mapped[A, B](path: Path[A], f: A => Option[B], g: B => A) extends Path[B] {
    override protected[simplerouter] def encode = b => path.encode(g(b))

    override protected[simplerouter] def parsePrefix: Location => Option[(B, Location)] =
      path.parsePrefix(_).flatMap { case (a, rest) => f(a).map(_ -> rest) }
  }

  /** `first` followed by `second`, as [[Path.++]] makes it */
  case class Concatenated[A, B, C](first: Path[A], second: Path[B], concat: TupleConcat[A, B, C]) extends Path[C] {
    override protected[simplerouter] def encode = c => {
      val (a, b) = concat.split(c)
      first.encode(a).andThen(second.encode(b))
    }

    override protected[simplerouter] def parsePrefix: Location => Option[(C, Location)] =
      first.parsePrefix(_).flatMap { case (a, rest) =>
        second.parsePrefix(rest).map { case (b, rest2) => (concat.join(a, b), rest2) }
      }
  }
}
