package simplerouter

import org.scalacheck.Gen
import org.scalacheck.util.Pretty

/** Generators of [[Location]]s and of the text in them */
object LocationGen {
  private val punctuation = " !\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~\t\n\r"

  /** Any Unicode scalar value, weighted toward ASCII punctuation, which is where encoding mistakes happen. Lone
    * surrogates are excluded because they have no UTF-8 encoding.
    */
  val codePoint: Gen[Int] =
    Gen.frequency(
      4 -> Gen.oneOf(punctuation).map(_.toInt),
      2 -> Gen.alphaNumChar.map(_.toInt),
      1 -> Gen.choose(0, 0xd7ff),
      1 -> Gen.choose(0xe000, 0x10ffff)
    )

  val text: Gen[String] = Gen.listOf(codePoint).map(_.flatMap(Character.toChars(_)).mkString)

  /** Weighted toward the segments `normalized` changes, which random text rarely produces. */
  val segment: Gen[String] = Gen.frequency(6 -> text, 1 -> Gen.oneOf("", ".", ".."))

  val path: Gen[List[String]] = Gen.listOf(segment)

  val query: Gen[List[(String, String)]] = Gen.listOf(Gen.zip(text, text))

  val location: Gen[Location] = Gen.zip(path, query).map { case (p, q) => Location(p, q) }

  /** Prints strings quoted, so that `List("")` doesn't print as `List()`. */
  implicit val prettyLocation: Location => Pretty = loc =>
    Pretty { _ =>
      def quote(s: String) = "\"" + s + "\""
      val query            = loc.query.map { case (k, v) => s"(${quote(k)}, ${quote(v)})" }
      s"Location(${loc.path.map(quote).mkString("List(", ", ", ")")}, ${query.mkString("List(", ", ", ")")})"
    }
}
