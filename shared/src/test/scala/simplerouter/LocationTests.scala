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
