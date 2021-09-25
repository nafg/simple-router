package simplerouter

import simplerouter.Tuples.{->, Any_->:}

private class Extractor[-A, +B](f: A => Option[B]) {
  def unapply(a: A): Option[B] = f(a)
}

/** A path is a typesafe URL template. It consists of a chain of typed components, (path components and query
  * parameters) which may be fixed strings, or string representations of some value
  *
  * @tparam A
  *   the type of data encoded in this `Path`
  */
sealed trait Path[A] {
  protected[simplerouter] def encode: A => Location => Location

  /** Returns the [[Location]] representing `param`
    */
  final def build(param: A): Location = encode(param)(Location(Nil))

  def parse: Location => Option[A]

  /** Given a route value, returns a `PartialFunction` that parses a [[Location]] and passes the parameters to the route
    * value, if applicable
    */
  final def run[R](handler: A => Option[R]): Location => Option[R] = parse(_).flatMap(handler)
}

object Path extends PathCompanionImplicits {
  val empty                  = Segment.Empty
  def apply(literal: String) = Segment.Literal(literal, empty)

  sealed trait Segment[A] extends Path[A]
  object Segment {

    /** Every [[Path]] chain ends with [[Empty]] (the empty `Path`), or [[All]] ([[syntax.**]]). There is only one
      * [[Empty]] instance, aliased as [[Empty]]. However, the DSL does not require you to actually write [[Empty]].
      */
    case object Empty extends Path.Segment[Unit] {
      override protected[simplerouter] def encode = _ => identity

      override def parse = {
        case Location(Nil, _) => Some(())
        case _                => None
      }
    }

    /** Every [[Path]] chain ends with [[Empty]], or [[All]]. `PAny` represents all the (remaining) url path components
      * as one `List[String]`. There is only one `PAny` instance, aliased as `PAny`.
      */
    case object All extends Path.Segment[List[String] -> Unit] {
      override protected[simplerouter] def encode = param => _ ++ param._1

      override def parse: Location => Option[(List[String], Unit)] = loc => Some(loc.path ->: ())
    }

    /** [[Literal]] is a fixed-string url path component. It is not converted to or from a value.
      */
    case class Literal[A](component: String, next: Path[A]) extends Path.Segment[A] {
      override protected[simplerouter] def encode = param => l => next.encode(param)(l :+ component)

      override def parse: Location => Option[A] = {
        case loc @ Location(`component` :: _, _) => next.parse(loc.tail)
        case _                                   => None
      }
    }

    /** [[Parameter]] is a url path component that is converted to and from a typed value. The actual conversion is
      * provided by `arg`.
      */
    case class Parameter[A, B](param: Param.SegmentParam[A], next: Path[B]) extends Path.Segment[A -> B] {
      override protected[simplerouter] def encode    = { case a -> b =>
        l => next.encode(b)(l :+ param.stringable.format(a))
      }

      override def parse: Location => Option[(A, B)] = {
        case loc @ Location(param.stringable(a) :: _, _) => next.parse(loc.tail).map(a -> _)
        case _                                           => None
      }
    }
  }

  /** Marker trait, used by the DSL so that `:&:` is used rather than `:/:` */
  sealed trait QueryParameter[A] extends Path[A]
  object QueryParameter {

    /** An optional named url query parameter that is converted to and from a typed value. The actual conversion is
      * provided by `arg`. The routing function receives None if the url does not contain the query parameter. However
      * if it contains it, but `param` does not parse it, then the `Path` does not match.
      */
    case class Optional[A, B](param: Param.QueryParam[A], next: Path[B]) extends QueryParameter[Option[A] -> B] {
      private val locParam = new Extractor((_: Location).takeParam(param.key))

      override protected[simplerouter] def encode            = { case ao -> b =>
        l =>
          val loc2 = ao match {
            case None    => l
            case Some(a) => l & ((param.key, param.stringable format a))
          }
          next.encode(b)(loc2)
      }

      override def parse: Location => Option[(Option[A], B)] = {
        case locParam(param.stringable(a), loc2) => next.parse(loc2).map(Some(a) -> _)
        case loc                                 => next.parse(loc).flatMap {
            case b if loc.query.forall(_._1 != param.key) => Some(None -> b)
            case _                                        => None
          }
      }
    }

    /** A repeatable named url query parameter, each occurrence of which is converted to and from a typed `List` of
      * values. The actual conversion is provided by `arg`.
      */
    case class Repeated[A, B](params: Param.QueryParamRep[A], next: Path[B]) extends QueryParameter[List[A] -> B] {
      private val locParams = new Extractor((loc: Location) => Some(loc.takeParams(params.key)))
      private val parseAll  = new Extractor((xs: List[String]) => Some(xs.flatMap(params.stringable.parse(_))))

      override protected[simplerouter] def encode          = { case as -> b =>
        loc =>
          val location = loc && ((params.key, as.map(params.stringable.format)))
          next.encode(b)(location)
      }

      override def parse: Location => Option[(List[A], B)] = {
        case locParams(parseAll(as), loc2) => next.parse(loc2).map(as -> _)
        case _                             => None
      }
    }
  }
}
