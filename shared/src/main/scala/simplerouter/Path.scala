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
  *   the value this path holds: its parameters' values as a right-nested tuple ending in `Unit`, e.g.
  *   `Int ->: Option[String] ->: Unit` (see [[TupleSyntax]])
  */
sealed trait Path[A] {
  protected[simplerouter] def encode: A => Location => Location

  /** [[parse]] without normalizing: the value `location` holds, as it is, if it matches this path */
  protected[simplerouter] def parseAsIs: Location => Option[A]

  /** The [[Location]] this path describes for `param` */
  final def build(param: A): Location = encode(param)(Location(Nil))

  /** The value `location` holds, if it matches this path. The path is matched against `location`'s
    * [[Location.normalized normalized]] form, and only if it accounts for every one of its path segments; query
    * parameters this path does not declare are ignored.
    */
  final def parse: Location => Option[A] = loc => parseNormalized(loc.normalized)

  /** [[parse]] for a `location` that is already [[Location.normalized normalized]] */
  protected[simplerouter] final def parseNormalized(location: Location): Option[A] = parseAsIs(location)

  /** A function that [[parse parses]] a [[Location]] and passes its value to `handler`. It returns `None` if the
    * location does not match or `handler` returns `None`.
    */
  final def run[R](handler: A => Option[R]): Location => Option[R] = loc => parse(loc).flatMap(handler)
}

object Path extends PathCompanionImplicits {

  /** The path with no segments, which is the root, `/` */
  val empty = Segment.Empty

  /** A path consisting of the single segment `literal`. The root is [[empty]].
    *
    * @throws java.lang.IllegalArgumentException
    *   if `literal` is empty. An empty string literal is rejected at compile time instead.
    */
  def apply(literal: String): Segment.Literal[Unit] = {
    require(literal.nonEmpty, "Path(\"\") is not a path segment: use Path.empty for the root")
    Segment.Literal(literal, empty)
  }

  /** Never compiles: it makes `Path("")` a compile error. The root is [[empty]]. */
  def apply(literal: "")(implicit noEmptyLiteral: NoEmptyLiteral): Nothing = ???

  /** Has no instances, so that writing an empty path literal fails to compile with this message */
  @implicitNotFound("\"\" is not a path segment: use Path.empty for the root")
  sealed abstract class NoEmptyLiteral

  /** A [[Path]] that starts with a path segment, or ends the chain */
  sealed trait Segment[A] extends Path[A]
  object Segment {

    /** The path with no segments, also available as [[Path.empty]]. It matches a [[Location]] with no path segments. A
      * chain ends with it unless it ends with [[All]]; the DSL adds it for you.
      */
    case object Empty extends Path.Segment[Unit] {
      override protected[simplerouter] def encode = _ => identity

      override protected[simplerouter] def parseAsIs = {
        case Location(Nil, _) => Some(())
        case _                => None
      }
    }

    /** Matches all the remaining path segments, which its part of the value holds as a `List[String]`. It ends a chain.
      * Also available as `syntax.**`.
      */
    case object All extends Path.Segment[List[String] -> Unit] {
      override protected[simplerouter] def encode = param => _ ++ param._1

      override protected[simplerouter] def parseAsIs: Location => Option[(List[String], Unit)] =
        loc => Some(loc.path ->: ())
    }

    /** The fixed path segment `component`, followed by `next`. It adds nothing to the path's value. */
    case class Literal[A](component: String, next: Path[A]) extends Path.Segment[A] {
      override protected[simplerouter] def encode = param => l => next.encode(param)(l :+ component)

      override protected[simplerouter] def parseAsIs: Location => Option[A] = {
        case loc @ Location(`component` :: _, _) => next.parseAsIs(loc.tail)
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

      override protected[simplerouter] def parseAsIs: Location => Option[(A, B)] = {
        case loc @ Location(param.stringable(a) :: _, _) => next.parseAsIs(loc.tail).map(a -> _)
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

      override protected[simplerouter] def parseAsIs: Location => Option[(Option[A], B)] = {
        case locParam(param.stringable(a), loc2) => next.parseAsIs(loc2).map(Some(a) -> _)
        case loc                                 => next.parseAsIs(loc).flatMap {
            case b if loc.query.forall(_._1 != param.key) => Some(None -> b)
            case _                                        => None
          }
      }
    }

    /** A query parameter that may occur any number of times, followed by `next`. Its part of the value is the list of
      * the values of every occurrence of `params.key`, in order, each converted by `params`'s [[Stringable]].
      * Occurrences that `params` cannot parse are left out of the list; they do not stop the location from matching.
      */
    case class Repeated[A, B](params: Param.QueryParamRep[A], next: Path[B]) extends QueryParameter[List[A] -> B] {
      private val locParams = new Extractor((loc: Location) => Some(loc.takeParams(params.key)))
      private val parseAll  = new Extractor((xs: List[String]) => Some(xs.flatMap(params.stringable.parse(_))))

      override protected[simplerouter] def encode = { case as -> b =>
        loc =>
          val location = loc && ((params.key, as.map(params.stringable.format)))
          next.encode(b)(location)
      }

      override protected[simplerouter] def parseAsIs: Location => Option[(List[A], B)] = {
        case locParams(parseAll(as), loc2) => next.parseAsIs(loc2).map(as -> _)
        case _                             => None
      }
    }
  }
}
