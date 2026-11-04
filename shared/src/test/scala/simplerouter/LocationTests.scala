package simplerouter

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class LocationTests extends AnyFunSuite with Matchers {
  test("toUrl") {
    val loc = Location(List("p@th", "to&", "re/source"), List(("f#eld", "v@lue"), ("ke%", "value?")))
    loc.toUrl should equal("/p%40th/to%26/re%2Fsource?f%23eld=v%40lue&ke%25=value%3F")
  }

  test("toUrl encodes a space in a path segment as %20") {
    Location(List("a b")).toUrl should equal("/a%20b")
  }

  test("normalized resolves . and .. segments") {
    Location(List("a", ".", "b")).normalized.path should equal(List("a", "b"))
    Location(List("a", "..", "b")).normalized.path should equal(List("b"))
    Location(List("..", "a")).normalized.path should equal(List("a"))
  }

  test("normalized leaves a trailing slash for a trailing . or ..") {
    Location(List("a", "b", ".")).normalized.path should equal(List("a", "b", ""))
    Location(List("a", "b", "..")).normalized.path should equal(List("a", ""))
  }

  test("normalized drops leading empty segments, so the root is Nil") {
    Location(List("")).normalized.path should equal(Nil)
    Location(List("", "", "x")).normalized.path should equal(List("x"))
    Location(List("a", "..")).normalized.path should equal(Nil)
  }

  test("normalized keeps empty segments after the first") {
    Location(List("a", "", "b")).normalized.path should equal(List("a", "", "b"))
    Location(List("a", "")).normalized.path should equal(List("a", ""))
  }

  test("normalized returns the same instance when there is nothing to resolve") {
    val loc = Location(List("a", "", "b"))
    loc.normalized should be theSameInstanceAs loc
  }

  test("toUrl renders the normalized path, so it never starts with //") {
    Location(List("", "evil.example")).toUrl should equal("/evil.example")
    Location(List("a", "..", "b")).toUrl should equal("/b")
  }

  test("parsePathAndQuery decodes the path and query and ignores the fragment") {
    Location.parsePathAndQuery("/a/b%20c?k=v%26w#f?x=y") should equal(Location(List("a", "b c"), List("k" -> "v&w")))
  }

  test("parsePathAndQuery parses the root as Nil") {
    Location.parsePathAndQuery("/") should equal(Location(Nil))
    Location.parsePathAndQuery("") should equal(Location(Nil))
  }

  test("parsePathAndQuery returns the normalized Location, resolving encoded dot segments too") {
    Location.parsePathAndQuery("/a/../%2E%2e/b/./c") should equal(Location(List("b", "c")))
    Location.parsePathAndQuery("//x/") should equal(Location(List("x", "")))
  }

  test("parsePathAndQuery keeps + in the path and decodes it as a space in the query") {
    Location.parsePathAndQuery("/a+b?a+b=c+d") should equal(Location(List("a+b"), List("a b" -> "c d")))
  }

  test("parseQuery separates parameters on & and not on ;") {
    Location.parseQuery("a=1;b=2&c") should equal(List("a" -> "1;b=2", "c" -> ""))
  }

  test("parseQuery drops one leading ?, skips empty parameters and splits on the first =") {
    Location.parseQuery("??a=b=c&&=d&") should equal(List("?a" -> "b=c", "" -> "d"))
  }

  test("parseQuery keeps a % that doesn't start an escape and replaces invalid UTF-8 with U+FFFD") {
    Location.parseQuery("a=%zz%4%&b=%C3%A9%FF") should equal(List("a" -> "%zz%4%", "b" -> "\u00e9\ufffd"))
  }

  test("parseQuery replaces each maximal invalid UTF-8 sequence with one U+FFFD") {
    Location.parseQuery("a=%E4%B8%F7%93") should equal(List("a" -> "\ufffd\ufffd\ufffd"))
  }

  test("parseQuery and parsePathAndQuery replace unpaired surrogates with U+FFFD") {
    Location.parseQuery("a=\ud800&b=\udc00+&c=\ud83d\ude00%41") should
      equal(List("a" -> "\ufffd", "b" -> "\ufffd ", "c" -> "\ud83d\ude00A"))
    Location.parsePathAndQuery("/\ud800/\udc00%41") should equal(Location(List("\ufffd", "\ufffdA")))
  }

  test("parsePathAndQueryStrict gives what parsePathAndQuery gives for a well-formed path and query") {
    for (raw <- List("/a/b%20c?k=v%26w#f", "/%EF%BF%BD", "/a?k=v#%zz", "/a+b?a+b=%E4%B8%AD", ""))
      Location.parsePathAndQueryStrict(raw) should equal(Some(Location.parsePathAndQuery(raw)))
    Location.parsePathAndQueryStrict("/%EF%BF%BD") should equal(Some(Location(List("\ufffd"))))
  }

  test("parsePathAndQueryStrict rejects malformed escapes, invalid UTF-8 and unpaired surrogates") {
    for (raw <- List("/%zz", "/%", "/a?k=%4", "/%FF", "/a?k=%C3", "/%C3/%A9", "/%ED%A0%80", "/\ud800", "/a?\udc00"))
      withClue(raw)(Location.parsePathAndQueryStrict(raw) should equal(None))
  }

  test("parseQueryStrict gives what parseQuery gives for a well-formed query") {
    for (raw <- List("a=1;b=2&c", "?a=%EF%BF%BD", "a+b=c+d&&=e", ""))
      Location.parseQueryStrict(raw) should equal(Some(Location.parseQuery(raw)))
  }

  test("parseQueryStrict rejects malformed escapes, invalid UTF-8 and unpaired surrogates") {
    for (raw <- List("a=%", "a=%zz", "%4=b", "a=%E4%B8", "a=%C0%80", "a=\udc00"))
      withClue(raw)(Location.parseQueryStrict(raw) should equal(None))
  }

  test("Path(\"\") does not compile") {
    assertCompiles("""Path("a")""")
    assertDoesNotCompile("""Path("")""")
  }

  test("\"\" :/: … does not compile") {
    assertCompiles("""import simplerouter.syntax.*; "x" :/: "a"""")
    assertDoesNotCompile("""import simplerouter.syntax.*; "" :/: "a"""")
  }

  test("\"\" :&: … does not compile") {
    assertCompiles("""import simplerouter.syntax.*; "x" :&: Param[Int]("a")""")
    assertDoesNotCompile("""import simplerouter.syntax.*; "" :&: Param[Int]("a")""")
  }

  test("Path, :/: and :&: reject an empty literal that isn't a compile-time constant") {
    val empty = ""
    an[IllegalArgumentException] should be thrownBy Path(empty)
    an[IllegalArgumentException] should be thrownBy {
      import simplerouter.syntax.*
      empty :/: "a"
    }
    an[IllegalArgumentException] should be thrownBy {
      import simplerouter.syntax.*
      empty :&: Param[Int]("a")
    }
  }
}
