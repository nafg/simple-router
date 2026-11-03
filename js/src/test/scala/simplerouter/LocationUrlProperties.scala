package simplerouter

import scala.scalajs.js
import scala.scalajs.js.URIUtils.decodeURIComponent

import org.scalacheck.Prop.*
import org.scalacheck.util.Pretty
import org.scalacheck.{Gen, Properties, Test}
import org.scalajs.dom.URL

/** Checks [[Location]] against the WHATWG URL parser, which Node shares with browsers. */
object LocationUrlProperties extends Properties("Location") {
  override def overrideParameters(p: Test.Parameters): Test.Parameters = p.withMinSuccessfulTests(2000)

  private val punctuation = " !\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~\t\n\r"

  /** Any Unicode scalar value, weighted toward ASCII punctuation, which is where encoding mistakes happen. Lone
    * surrogates are excluded because they have no UTF-8 encoding.
    */
  private val codePoint: Gen[Int] =
    Gen.frequency(
      4 -> Gen.oneOf(punctuation).map(_.toInt),
      2 -> Gen.alphaNumChar.map(_.toInt),
      1 -> Gen.choose(0, 0xd7ff),
      1 -> Gen.choose(0xe000, 0x10ffff)
    )

  private val text: Gen[String] = Gen.listOf(codePoint).map(_.flatMap(Character.toChars(_)).mkString)

  /** Weighted toward the segments `normalized` changes, which random text rarely produces. */
  private val segment: Gen[String] = Gen.frequency(6 -> text, 1 -> Gen.oneOf("", ".", ".."))

  private val path: Gen[List[String]] = Gen.listOf(segment)

  private val query: Gen[List[(String, String)]] = Gen.listOf(Gen.zip(text, text))

  /** Prints strings quoted, so that `List("")` doesn't print as `List()`. */
  implicit val prettyLocation: Location => Pretty = loc =>
    Pretty { _ =>
      def quote(s: String) = "\"" + s + "\""
      val query            = loc.query.map { case (k, v) => s"(${quote(k)}, ${quote(v)})" }
      s"Location(${loc.path.map(quote).mkString("List(", ", ", ")")}, ${query.mkString("List(", ", ", ")")})"
    }

  private def parse(url: String): URL = new URL(url, "http://example.com")

  private def segments(url: URL): List[String] = url.pathname.drop(1).split("/", -1).toList.map(decodeURIComponent)

  private def toLocation(url: URL): Location =
    Location(segments(url), js.Array.from(url.searchParams).toList.map(kv => kv._1 -> kv._2)).normalized

  property("toUrl produces a URL that parses back to the normalized Location") = forAll(path, query) { (path, query) =>
    val loc = Location(path, query)
    toLocation(parse(loc.toUrl)) ?= loc.normalized
  }

  /** The URL keeps `.` and `..` as they are for the parser to resolve. The `/.` prefix stops a leading empty segment
    * from starting `//`, which the parser would read as a host. Unlike the parser, `normalized` also drops leading
    * empty segments, so that `toUrl` never renders `//`.
    */
  property("normalized resolves . and .. like the WHATWG URL parser, then drops leading empty segments") =
    forAll(path) { path =>
      val url = "/." + path.map(s => "/" + (if (s == "." || s == "..") s else Location.encodeComponent(s))).mkString
      segments(parse(url)).dropWhile(_.isEmpty) ?= Location(path).normalized.path
    }
}
