package simplerouter

import java.nio.charset.StandardCharsets

/** A URL path and query string, held as decoded path segments and query parameters.
  *
  * @param path
  *   the path segments, decoded, without the `/` separators: `/a/b%20c` is `List("a", "b c")`, and the root is `Nil`
  * @param query
  *   the query parameters as decoded key-value pairs. A key may occur more than once.
  */
case class Location(path: List[String], query: List[(String, String)] = Nil) {

  /** Append path components */
  def ++(xs: List[String]) = copy(path = path ++ xs)

  /** Append a path component */
  def :+(append: String) = copy(path = path :+ append)

  /** Prepend a path component */
  def +:(prepend: String) = copy(path = prepend +: path)

  /** Drop the first path component */
  def tail = copy(path = path.tail)

  /** Add a query string parameter, before the existing ones */
  def &(x: (String, String)) = copy(query = x :: query)

  /** Add multiple query string parameters with the same name, before the existing ones */
  def &&(x: (String, List[String])) = copy(query = x._2.map(x._1 -> _) ::: query)

  /** The first value of the query parameter `name`, and this `Location` without that occurrence; `None` if there is no
    * parameter `name`
    */
  def takeParam(name: String): Option[(String, Location)] = query.span(_._1 != name) match {
    case (_, Nil)                => None
    case (notIt, (_, v) :: rest) => Some(v -> copy(query = notIt ++ rest))
  }

  /** Every value of the query parameter `name`, in order, and this `Location` without them */
  def takeParams(name: String): (List[String], Location) = {
    val (it, notIt) = query.partition(_._1 == name)
    (it.map(_._2), copy(query = notIt))
  }

  /** This `Location` with its path resolved the way a browser resolves it before sending a request: `.` and `..`
    * segments are resolved (`..` above the root stays at the root), and leading empty segments are dropped, so the root
    * is `Nil`. Two `Location`s refer to the same resource when their normalized forms are equal.
    */
  def normalized: Location =
    if (path.headOption.contains("") || path.exists(s => s == "." || s == "..")) copy(path = Location.normalize(path))
    else this

  /** This `Location`'s [[normalized]] path and its query as a URL-encoded string, starting with `/`, e.g.
    * `/a/b%20c?k=v`. Each segment, key and value is encoded with [[Location.encodeComponent]].
    */
  def toUrl = {
    val q =
      if (query.isEmpty) ""
      else
        "?" + query.map { case (k, v) => Location.encodeComponent(k) + "=" + Location.encodeComponent(v) }.mkString("&")
    normalized.path.map(Location.encodeComponent).mkString("/", "/", q)
  }
}

object Location {

  /** The [[Location]] of a path, optionally followed by `?` and a query string and by `#` and a fragment, as in an HTTP
    * request target. The path is split on `/` and each segment is decoded as [[parseQuery]] decodes a key, except that
    * `+` stays `+`. The query string is parsed with [[parseQuery]], and the fragment is ignored. The result is
    * [[Location.normalized normalized]], so `parsePathAndQuery(loc.toUrl) == loc.normalized` for every `loc` whose
    * strings have no unpaired surrogates.
    */
  def parsePathAndQuery(raw: String): Location = decodePathAndQuery(raw).value

  /** [[parsePathAndQuery]] of `raw`, or `None` if the part of `raw` before any `#` has a `%` that isn't followed by two
    * hex digits, an unpaired surrogate, or a path segment, key or value whose decoded bytes aren't valid UTF-8
    */
  def parsePathAndQueryStrict(raw: String): Option[Location] = decodePathAndQuery(raw).toOption

  /** The parameters of the query string `raw`, parsed as the WHATWG URL standard's `URLSearchParams` parses it. A
    * leading `?` is dropped. Parameters are separated by `&`, and not by `;`, which can occur in a key or value. Empty
    * parameters are skipped. The first `=` in a parameter separates the key from the value, which is `""` if there is
    * no `=`. In keys and values, `+` is a space, `%` followed by two hex digits is the byte they encode, and the bytes
    * are decoded as UTF-8. A `%` that isn't followed by two hex digits stays `%`, and each maximal invalid UTF-8
    * sequence becomes one U+FFFD, as the WHATWG Encoding standard specifies. An unpaired surrogate in `raw` becomes
    * U+FFFD too.
    */
  def parseQuery(raw: String): List[(String, String)] = decodeQuery(raw).value

  /** [[parseQuery]] of `raw`, or `None` if `raw` has a `%` that isn't followed by two hex digits, an unpaired
    * surrogate, or a key or value whose decoded bytes aren't valid UTF-8
    */
  def parseQueryStrict(raw: String): Option[List[(String, String)]] = decodeQuery(raw).toOption

  /** A decoded value, and whether the text it was decoded from was well formed. Malformed text is decoded too, with
    * what's malformed in it kept or replaced by U+FFFD.
    */
  private final case class Decoded[+A](value: A, wellFormed: Boolean) {
    def map[B](f: A => B): Decoded[B]             = Decoded(f(value), wellFormed)
    def zip[B](that: Decoded[B]): Decoded[(A, B)] = Decoded((value, that.value), wellFormed && that.wellFormed)
    def toOption: Option[A]                       = Option.when(wellFormed)(value)
  }

  private object Decoded {
    def sequence[A](xs: List[Decoded[A]]): Decoded[List[A]] = Decoded(xs.map(_.value), xs.forall(_.wellFormed))
  }

  private def decodePathAndQuery(raw: String): Decoded[Location] = {
    val (path, query) = raw.takeWhile(_ != '#').span(_ != '?')
    Decoded.sequence(path.split("/", -1).toList.map(percentDecode(_, plusIsSpace = false)))
      .zip(decodeQuery(query))
      .map { case (segments, params) => Location(segments, params).normalized }
  }

  private def decodeQuery(raw: String): Decoded[List[(String, String)]] =
    Decoded.sequence(raw.stripPrefix("?").split("&", -1).toList.filter(_.nonEmpty).map { param =>
      val (key, value) = param.span(_ != '=')
      percentDecode(key, plusIsSpace = true).zip(percentDecode(value.drop(1), plusIsSpace = true))
    })

  private def percentDecode(raw: String, plusIsSpace: Boolean): Decoded[String] = {
    val scalarValues = replaceUnpairedSurrogates(raw)
    val s            = scalarValues.value
    if (s.indexOf('%') < 0 && !(plusIsSpace && s.indexOf('+') >= 0)) scalarValues
    else {
      // UTF-8 encodes non-ASCII characters with non-ASCII bytes only, so the ASCII `%`, hex digits and `+` can be
      // found among the bytes
      val bytes             = s.getBytes(StandardCharsets.UTF_8)
      val out               = new java.io.ByteArrayOutputStream(bytes.length)
      var escapesWellFormed = true
      var i                 = 0
      while (i < bytes.length) {
        val b = bytes(i)
        if (b == '%' && i + 2 < bytes.length && hexValue(bytes(i + 1)) >= 0 && hexValue(bytes(i + 2)) >= 0) {
          out.write(hexValue(bytes(i + 1)) * 16 + hexValue(bytes(i + 2)))
          i += 3
        } else {
          if (b == '%') escapesWellFormed = false
          out.write(if (plusIsSpace && b == '+') ' '.toInt else b.toInt)
          i += 1
        }
      }
      val utf8              = decodeUtf8(out.toByteArray)
      Decoded(utf8.value, scalarValues.wellFormed && escapesWellFormed && utf8.wellFormed)
    }
  }

  /** `s` with each unpaired surrogate replaced by U+FFFD, as the WHATWG URL standard converts a string to Unicode
    * scalar values; it is well formed if it had none
    */
  private def replaceUnpairedSurrogates(s: String): Decoded[String] =
    if (!s.exists(Character.isSurrogate)) Decoded(s, wellFormed = true)
    else {
      val sb       = new java.lang.StringBuilder(s.length)
      var replaced = false
      var i        = 0
      while (i < s.length) {
        val c = s.charAt(i)
        if (Character.isHighSurrogate(c) && i + 1 < s.length && Character.isLowSurrogate(s.charAt(i + 1))) {
          sb.append(c).append(s.charAt(i + 1))
          i += 2
        } else {
          if (Character.isSurrogate(c)) {
            sb.append('\ufffd')
            replaced = true
          } else sb.append(c)
          i += 1
        }
      }
      Decoded(sb.toString, wellFormed = !replaced)
    }

  /** Decodes `bytes` with the WHATWG Encoding standard's UTF-8 decoder, which replaces each maximal invalid subsequence
    * with U+FFFD; the result is well formed if `bytes` is valid UTF-8. Platforms' own decoders don't all do the same.
    */
  private def decodeUtf8(bytes: Array[Byte]): Decoded[String] = {
    val sb          = new java.lang.StringBuilder(bytes.length)
    var valid       = true
    var codePoint   = 0
    var bytesNeeded = 0
    var bytesSeen   = 0
    var lower       = 0x80
    var upper       = 0xbf
    var i           = 0
    def replace()   = {
      sb.append('\ufffd')
      valid = false
    }
    while (i < bytes.length) {
      val b = bytes(i) & 0xff
      if (bytesNeeded == 0) {
        if (b <= 0x7f) sb.append(b.toChar)
        else if (b >= 0xc2 && b <= 0xdf) {
          bytesNeeded = 1
          codePoint = b & 0x1f
        } else if (b >= 0xe0 && b <= 0xef) {
          if (b == 0xe0) lower = 0xa0
          if (b == 0xed) upper = 0x9f
          bytesNeeded = 2
          codePoint = b & 0xf
        } else if (b >= 0xf0 && b <= 0xf4) {
          if (b == 0xf0) lower = 0x90
          if (b == 0xf4) upper = 0x8f
          bytesNeeded = 3
          codePoint = b & 0x7
        } else replace()
        i += 1
      } else if (b < lower || b > upper) {
        // The sequence so far is invalid; `b` is decoded again as the start of the next one
        replace()
        codePoint = 0
        bytesNeeded = 0
        bytesSeen = 0
        lower = 0x80
        upper = 0xbf
      } else {
        lower = 0x80
        upper = 0xbf
        codePoint = (codePoint << 6) | (b & 0x3f)
        bytesSeen += 1
        if (bytesSeen == bytesNeeded) {
          sb.appendCodePoint(codePoint)
          codePoint = 0
          bytesNeeded = 0
          bytesSeen = 0
        }
        i += 1
      }
    }
    if (bytesNeeded != 0) replace()
    Decoded(sb.toString, valid)
  }

  /** The value of the ASCII hex digit `b`, or -1 if it isn't one */
  private def hexValue(b: Byte): Int =
    if (b >= '0' && b <= '9') b - '0'
    else if (b >= 'a' && b <= 'f') b - 'a' + 10
    else if (b >= 'A' && b <= 'F') b - 'A' + 10
    else -1

  private def normalize(path: List[String]): List[String] = {
    val lastIndex = path.length - 1
    // A trailing `.` or `..` leaves a trailing slash, as `/a/b/..` resolves to `/a/`
    val resolved  = path.zipWithIndex.foldLeft(Vector.empty[String]) {
      case (acc, (".", i))  => if (i == lastIndex) acc :+ "" else acc
      case (acc, ("..", i)) => if (i == lastIndex) acc.dropRight(1) :+ "" else acc.dropRight(1)
      case (acc, (s, _))    => acc :+ s
    }
    resolved.dropWhile(_.isEmpty).toList
  }

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
    (c >= 'a' && c <= 'z') ||
      (c >= 'A' && c <= 'Z') ||
      (c >= '0' && c <= '9') || "-_.!~*'()".indexOf(c.toInt) >= 0
}
