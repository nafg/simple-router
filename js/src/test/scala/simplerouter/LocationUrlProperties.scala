package simplerouter

import scala.scalajs.js
import scala.scalajs.js.URIUtils.decodeURIComponent
import scala.util.Try
import scala.util.matching.Regex

import org.scalacheck.Prop.*
import org.scalacheck.{Gen, Properties, Test}
import org.scalajs.dom.{URL, URLSearchParams}

import LocationGen.prettyLocation

/** Checks [[Location]] against the WHATWG URL parser, which Node shares with browsers. */
object LocationUrlProperties extends Properties("Location") {
  override def overrideParameters(p: Test.Parameters): Test.Parameters = p.withMinSuccessfulTests(2000)

  /** Text that may contain percent-encoded bytes, valid UTF-8 or not, and the characters that delimit the parts of a
    * URL
    */
  private val rawText: Gen[String] =
    Gen.listOf(
      Gen.frequency(
        4 -> LocationGen.codePoint.map(cp => new String(Character.toChars(cp))),
        2 -> Gen.choose(0, 255).map(b => f"%%$b%02X"),
        1 -> Gen.oneOf("%", "%2", "%2e", "%2E%2e", "/", "?", "#", "&", "=", "+", ".", "..")
      )
    ).map(_.mkString)

  private def parse(url: String): URL = new URL(url, "http://example.com")

  private def params(searchParams: URLSearchParams): List[(String, String)] =
    js.Array.from(searchParams).toList.map(kv => kv._1 -> kv._2)

  /** Decodes `s` the way `URLSearchParams` decodes a value, except that `+` stays `+` */
  private def decode(s: String): String =
    new URLSearchParams("v=" + s.replace("+", "%2B").replace("&", "%26")).get("v")

  private def segments(url: URL): List[String] = url.pathname.drop(1).split("/", -1).toList.map(decode)

  private def toLocation(url: URL): Location = Location(segments(url), params(url.searchParams)).normalized

  property("toUrl produces a URL that parses back to the normalized Location") =
    forAll(LocationGen.path, LocationGen.query) { (path, query) =>
      val loc = Location(path, query)
      toLocation(parse(loc.toUrl)) ?= loc.normalized
    }

  /** The URL keeps `.` and `..` as they are for the parser to resolve. The `/.` prefix stops a leading empty segment
    * from starting `//`, which the parser would read as a host. Unlike the parser, `normalized` also drops leading
    * empty segments, so that `toUrl` never renders `//`.
    */
  property("normalized resolves . and .. like the WHATWG URL parser, then drops leading empty segments") =
    forAll(LocationGen.path) { path =>
      val url = "/." + path.map(s => "/" + (if (s == "." || s == "..") s else Location.encodeComponent(s))).mkString
      segments(parse(url)).dropWhile(_.isEmpty) ?= Location(path).normalized.path
    }

  /** The text is appended to an origin so that it can't change the origin. What follows the origin is the path, query
    * and fragment the parser serializes.
    */
  property("parsePathAndQuery parses what follows the origin as the WHATWG URL parser parses the URL") =
    forAll(rawText) { text =>
      val url = new URL("http://example.com/" + text)
      Location.parsePathAndQuery(url.href.stripPrefix(url.origin)) ?= toLocation(url)
    }

  /** `URLSearchParams` is given the query string with its non-ASCII characters percent-encoded, which doesn't change
    * what it decodes to. Node's `URLSearchParams` decodes a non-ASCII character next to an invalid escape wrongly: it
    * decodes `中%E4%B8` to `-\ufffd` instead of `中\ufffd`.
    */
  property("parseQuery parses like URLSearchParams") = forAll(rawText) { text =>
    val nonAscii = "[^\\x00-\\x7f]+".r
    val escaped  = nonAscii.replaceAllIn(text, m => Regex.quoteReplacement(Location.encodeComponent(m.matched)))
    Location.parseQuery(text) ?= params(new URLSearchParams(escaped))
  }

  /** `decodeURIComponent` throws for exactly the text the strict parsers reject: a `%` that isn't followed by two hex
    * digits, or escapes whose bytes aren't valid UTF-8. A multi-byte sequence can't span a delimiter, which is a
    * literal character, so decoding the text whole agrees with decoding each of its parts. `rawText` has no unpaired
    * surrogates.
    */
  private def wellFormed(text: String) = Try(decodeURIComponent(text)).isSuccess

  property("parsePathAndQueryStrict is parsePathAndQuery when the text before # is well formed, else None") =
    forAll(rawText) { text =>
      val ok = wellFormed(text.takeWhile(_ != '#'))
      classify(ok, "well formed", "malformed") {
        Location.parsePathAndQueryStrict(text) ?= Option.when(ok)(Location.parsePathAndQuery(text))
      }
    }

  property("parseQueryStrict is parseQuery when the text is well formed, else None") = forAll(rawText) { text =>
    val ok = wellFormed(text)
    classify(ok, "well formed", "malformed") {
      Location.parseQueryStrict(text) ?= Option.when(ok)(Location.parseQuery(text))
    }
  }
}
