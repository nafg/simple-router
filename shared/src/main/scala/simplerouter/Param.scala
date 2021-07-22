package simplerouter
import simplerouter.Tuples.->:

object Param {
  case class SegmentParam[A]()(implicit val stringable: Stringable[A])
  object SegmentParam extends ImplicitsForTuple2Path[λ[a => a]]
      with ImplicitsForToSegment[->:[*, Unit]] {
    override protected type From[A] = SegmentParam[A]
    override def toPath[A](from: SegmentParam[A]): Path.Segment.Parameter[A, Unit] =
      Path.Segment.Parameter(from, Path.empty)
  }

  case class QueryParam[A](key: String)(implicit val stringable: Stringable[A])
  object QueryParam extends ImplicitsForTuple2Path[Option]
      with ImplicitsForToQueryParameter[λ[a => (Option[a], Unit)]] {
    override protected type From[A] = QueryParam[A]
    override def toPath[A](from: QueryParam[A]): Path.QueryParameter.Optional[A, Unit] =
      Path.QueryParameter.Optional(from, Path.empty)
  }

  case class QueryParamRep[A](key: String)(implicit val stringable: Stringable[A])
  object QueryParamRep extends ImplicitsForTuple2Path[List]
      with ImplicitsForToQueryParameter[λ[a => (List[a], Unit)]] {
    override protected type From[A] = QueryParamRep[A]
    override def toPath[A](from: QueryParamRep[A]): Path.QueryParameter.Repeated[A, Unit] =
      Path.QueryParameter.Repeated(from, Path.empty)
  }

  /** Declare a path component that is converted to and from a value
    *
    * @example
    *   {{{"main" :/: Param[Int]   // e.g. /main/10}}}
    */
  def apply[A: Stringable]: SegmentParam[A] = SegmentParam[A]()

  /** Declare a url query parameter that is converted to and from a value
    *
    * @param key
    *   the query parameter key
    * @example
    *   {{{"main" :/: Param[Int]("item")   // e.g. /main?item=10}}}
    */
  def apply[A: Stringable](key: String) = new QueryParam[A](key)

  /** Declare a repeatable url query parameter that is converted to and from a value
    *
    * @param key
    *   the query parameter key
    * @example
    *   {{{"main" :/: param[Int]("options")   // e.g. /main/options=10&options=20}}}
    */
  def repeatable[A: Stringable](key: String) = new QueryParamRep[A](key)
}
