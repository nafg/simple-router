package simplerouter

import org.scalatest.Inside
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import simplerouter.syntax.{**, Any_->:, PathSegmentSyntax}

class RoutingTests extends AnyFunSuite with Matchers with Inside {
  val p  = "add" :/: Param[Int] :/: "plus" :/: Param[Int] :/: "please"
  val p2 = "add-all" :/: **
  val p3 = "test" :/: Param[Int] :&: Param[Int]("test1") :&: Param[Int]("test2") :&:
    Param.repeatable[Int]("test3")

  val p4 = "test" :/: Param[Int]
  val p5 = "test" :&: Param[Int]("test1")

  val r  = p >> { a => b => a + b }
  val r2 = p2 >> { xs => xs.flatMap(implicitly[Stringable[Int]].parse(_)).sum }
  val r3 = p3 >> { x => test1 => test2 => test3 => (x, test1, test2, test3) }

  test("Constructing links") {
    val c = p.construct(10, 30)
    assert(c == p.build(10 ->: 30 ->: ()))
    c.path should equal("add" :: "10" :: "plus" :: "30" :: "please" :: Nil)
    c.query should equal(Nil)

    val c2 = p2.construct("1" :: "2" :: Nil)
    assert(c2 == p2.build(("1" :: "2" :: Nil, ())))
    c2.path should equal("add-all" :: "1" :: "2" :: Nil)
    c2.query should equal(Nil)

    val c3 = p3.construct(10, Some(20), Some(30), List(1, 2, 3, 4))
    assert(c3 == p3.build(10 ->: Some(20) ->: Some(30) ->: List(1, 2, 3, 4) ->: ()))
    c3.path should equal("test" :: "10" :: Nil)
    c3.query should
      equal(List(("test3", "1"), ("test3", "2"), ("test3", "3"), ("test3", "4"), ("test2", "30"), ("test1", "20")))
  }

  test("Running urls") {
    r.run(Location("add" :: "10" :: "plus" :: "20" :: "please" :: Nil)) should equal(30)
    r.run.isDefinedAt(Location("add" :: "10" :: "and" :: "20" :: "please" :: Nil)) should equal(false)
    r.run.isDefinedAt(Location("add" :: "10" :: "plus" :: "20" :: Nil)) should equal(false)

    List(
      Location(Nil),
      Location("add-all" :: Nil),
      Location("add-all" :: "1" :: Nil),
      Location("add-all" :: "1" :: "2" :: Nil),
      Location("add-all" :: "1" :: "2" :: "3" :: Nil)
    ).map(r2.run.lift) should equal(List(None, Some(0), Some(1), Some(3), Some(6)))

    r3.run(Location(
      "test" :: "10" :: Nil,
      List("test1" -> "20", "test2" -> "30", "test2" -> "40", "test3" -> "1", "test3" -> "2", "test3" -> "a")
    )) should equal((10, Some(20), Some(30), List(1, 2)))
    r3.run.isDefinedAt(Location(
      "test" :: "10" :: Nil,
      List("test1" -> "20", "test2" -> "X30", "test2" -> "40", "test3" -> "1", "test3" -> "2")
    )) should equal(false)
  }

  test("Sites") {
    val s = r & r2 & r3

    List(
      Location("add" :: "10" :: "plus" :: "20" :: "please" :: Nil),
      Location("add" :: "10" :: "and" :: "20" :: "please" :: Nil),
      Location("add" :: "10" :: "plus" :: "20" :: Nil),
      Location(Nil),
      Location("add-all" :: Nil),
      Location("add-all" :: "1" :: Nil),
      Location("add-all" :: "1" :: "2" :: Nil),
      Location("add-all" :: "1" :: "2" :: "3" :: Nil),
      Location(
        "test" :: "10" :: Nil,
        List("test1" -> "20", "test2" -> "30", "test2" -> "40", "test3" -> "1", "test3" -> "2", "test3" -> "a")
      ),
      Location(
        "test" :: "10" :: Nil,
        List("test1" -> "20", "test2" -> "X30", "test2" -> "40", "test3" -> "1", "test3" -> "2")
      )
    ).map(s.run.lift) should equal(List(
      Some(30),
      None,
      None,
      None,
      Some(0),
      Some(1),
      Some(3),
      Some(6),
      Some((10, Some(20), Some(30), List(1, 2))),
      None
    ))
  }

  test("map") {
    val site = r & r
    val z    = site map ("000" + _)

    val res = z.run(p.build(10 ->: 20 ->: ()))

    implicitly[res.type <:< String]

    res should equal("00030")
  }

  test("map with various Path types") {
    val site2 = r & r2
    val z2    = site2 map ("000" + _)
    val res2  = z2 run Location(List("add", "15", "plus", "20", "please"))
    val res3  = z2 run Location(List("add-all", "10", "15", "20", "25"))
    implicitly[res2.type <:< String]
    implicitly[res3.type <:< String]

    res2 should equal("00035")
    res3 should equal("00070")
  }

  test("Can & together sitelets of different types") {
    val s1 = "a" :/: Param[Int] >>*? { case (i, ()) => i }
    val s2 = "b" :/: Param[String] >>*? { case (i, ()) => i }
    val s3 = "c" :/: Param[Boolean] >>*? { case (i, ()) => i }
    val s4 = s1 & s2 & s3

    s4.run(Location("a" :: "10" :: Nil)) shouldBe 10
    s4.run(Location("b" :: "10" :: Nil)) shouldBe "10"
    s4.run(Location("c" :: "true" :: Nil)) shouldBe true
  }

  test("Can & together sitelets, not only Sitelet&PathRoute") {
    val rt1     = "plus1" :/: Param[Int] >>* (_._1 + 1)
    val rt2     = "plus2" :/: Param[Int] >>* (_._1 + 2)
    val rt3     = "plus3" :/: Param[Int] >>* (_._1 + 3)
    val s1      = rt2 & rt3
    val sitelet = rt1 & s1
    sitelet.run(Location("plus1" :: "10" :: Nil)) shouldBe 11
  }

  test("Route can be a PartialFunction") {
    val rt = Param[Int] >>*? { case (i, ()) if i > 10 => i * 2 }
    rt.run.isDefinedAt(Location("10" :: Nil)) shouldBe false
    rt.run.isDefinedAt(Location("11" :: Nil)) shouldBe true
    rt.run.apply(Location("11" :: Nil)) shouldBe 22
  }
}
