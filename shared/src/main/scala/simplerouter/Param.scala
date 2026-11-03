package simplerouter
import simplerouter.Tuples.->:

/** Declares the typed parts of a [[Path]]: path segments and query parameters whose text is converted to and from a
  * value by a [[Stringable]].
  */
object Param {

  /** A path segment holding a value of type `A`. Create one with `Param[A]`. */
  case class SegmentParam[A]()(implicit val stringable: Stringable[A])
  object SegmentParam extends ImplicitsForTuple2Path[λ[a => a]]
      with ImplicitsForToSegment[->:[_, Unit]] {
    override protected type From[A] = SegmentParam[A]
    override def toPath[A](from: SegmentParam[A]): Path.Segment.Parameter[A, Unit] =
      Path.Segment.Parameter(from, Path.empty)
  }

  /** An optional query parameter named `key`, holding a value of type `A`. Create one with `Param[A](key)`. */
  case class QueryParam[A](key: String)(implicit val stringable: Stringable[A])
  object QueryParam extends ImplicitsForTuple2Path[Option]
      with ImplicitsForToQueryParameter[λ[a => (Option[a], Unit)]] {
    override protected type From[A] = QueryParam[A]
    override def toPath[A](from: QueryParam[A]): Path.QueryParameter.Optional[A, Unit] =
      Path.QueryParameter.Optional(from, Path.empty)
  }

  /** A query parameter named `key` that may occur any number of times, each occurrence holding a value of type `A`.
    * Create one with [[Param.repeatable]] or [[Param.repeatableStrict]].
    *
    * @param strict
    *   whether an occurrence that `stringable` cannot parse stops a location from matching. If not, it is ignored.
    */
  case class QueryParamRep[A](key: String, strict: Boolean = false)(implicit val stringable: Stringable[A]) {

    /** The values `texts` hold, in order: `None` if this is strict and one of them does not parse */
    private[simplerouter] def parseAll(texts: List[String]): Option[List[A]] =
      if (!strict) Some(texts.flatMap(stringable.parse(_)))
      else {
        val parsed = texts.map(stringable.parse(_))
        if (parsed.forall(_.isDefined)) Some(parsed.flatten) else None
      }
  }
  object QueryParamRep extends ImplicitsForTuple2Path[List]
      with ImplicitsForToQueryParameter[λ[a => (List[a], Unit)]] {
    override protected type From[A] = QueryParamRep[A]
    override def toPath[A](from: QueryParamRep[A]): Path.QueryParameter.Repeated[A, Unit] =
      Path.QueryParameter.Repeated(from, Path.empty)
  }

  /** Declares a path segment that is converted to and from a value. Its part of the path's value is an `A`.
    *
    * @example
    *   {{{"main" :/: Param[Int]   // e.g. /main/10}}}
    */
  def apply[A: Stringable]: SegmentParam[A] = SegmentParam[A]()

  /** Declares an optional query parameter that is converted to and from a value. Its part of the path's value is an
    * `Option[A]`.
    *
    * @param key
    *   the query parameter's name
    * @example
    *   {{{"main" :&: Param[Int]("item")   // e.g. /main?item=10}}}
    */
  def apply[A: Stringable](key: String) = new QueryParam[A](key)

  /** Declares a query parameter that may occur any number of times, each occurrence converted to and from a value. Its
    * part of the path's value is a `List[A]`.
    *
    * @param key
    *   the query parameter's name
    * @example
    *   {{{"main" :&: Param.repeatable[Int]("option")   // e.g. /main?option=10&option=20}}}
    */
  def repeatable[A: Stringable](key: String) = new QueryParamRep[A](key)

  /** Declares a query parameter that may occur any number of times, as [[repeatable]] does, except that a location with
    * an occurrence that does not parse does not match. [[repeatable]] leaves such an occurrence out of the list.
    *
    * @param key
    *   the query parameter's name
    * @example
    *   {{{"main" :&: Param.repeatableStrict[Int]("option")   // /main?option=10&option=x does not match}}}
    */
  def repeatableStrict[A: Stringable](key: String) = new QueryParamRep[A](key, strict = true)
}
