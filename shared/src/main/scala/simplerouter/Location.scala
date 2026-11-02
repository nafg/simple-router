package simplerouter

import java.nio.charset.StandardCharsets

case class Location(path: List[String], query: List[(String, String)] = Nil) {

  /** Append path components */
  def ++(xs: List[String]) = copy(path = path ++ xs)

  /** Append a path component */
  def :+(append: String) = copy(path = path :+ append)

  /** Prepend a path component */
  def +:(prepend: String) = copy(path = prepend +: path)

  /** Drop the first path component */
  def tail = copy(path = path.tail)

  /** Add a query string parameter */
  def &(x: (String, String)) = copy(query = x :: query)

  /** Add multiple query string parameters with the same name */
  def &&(x: (String, List[String])) = copy(query = x._2.map(x._1 -> _) ::: query)

  /** Get a `Tuple2` containing the named parameter's value, and the rest of this `Location` */
  def takeParam(name: String): Option[(String, Location)] = query.span(_._1 != name) match {
    case (_, Nil)                => None
    case (notIt, (_, v) :: rest) => Some(v -> copy(query = notIt ++ rest))
  }

  /** Get a `Tuple2` containing the named parameter's values, and the rest of this `Location` */
  def takeParams(name: String): (List[String], Location) = {
    val (it, notIt) = query.partition(_._1 == name)
    (it.map(_._2), copy(query = notIt))
  }

  /** Get a URL-encoded `String` representing this `Location` */
  def toUrl = {
    val q =
      if (query.isEmpty) ""
      else
        "?" + query.map { case (k, v) => Location.encodeComponent(k) + "=" + Location.encodeComponent(v) }.mkString("&")
    path.map(Location.encodeComponent).mkString("/", "/", q)
  }
}

object Location {
  /** Percent-encodes `s` as UTF-8 for use as one path segment, query key or query value, leaving only the characters
    * JavaScript's `encodeURIComponent` leaves: ASCII letters and digits and `-_.!~*'()`. The result is the same on
    * every platform.
    */
  def encodeComponent(s: String): String = {
    val sb = new java.lang.StringBuilder
    s.getBytes(StandardCharsets.UTF_8).foreach { b =>
      val c = (b & 0xff).toChar
      if (isLeftAsIs(c)) sb.append(c)
      else sb.append('%').append(HexDigits.charAt((b >> 4) & 0xf)).append(HexDigits.charAt(b & 0xf))
    }
    sb.toString
  }

  private val HexDigits = "0123456789ABCDEF"

  private def isLeftAsIs(c: Char) =
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || "-_.!~*'()".indexOf(c.toInt) >= 0
}
