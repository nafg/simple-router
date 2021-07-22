package simplerouter

object MinimalImportsCompilationTest {
  {
    Param[Int] >>* (_._1 + 1)
    Param[Int] >>* { case a -> () => a + 1 }
    Param[Int] >>*? { case a -> () => a + 1 }
    Param[Int] >> (_ + 1)

    Param[Int]("p") >>* (_._1.sum + 1)
    Param[Int]("p") >>* { case a -> () => a.sum + 1 }
    Param[Int]("p") >>*? { case a -> () => a.sum + 1 }
    Param[Int]("p") >> (_.sum + 1)

    Param.repeatable[Int]("p") >>* (_._1.sum + 1)
    Param.repeatable[Int]("p") >>* { case a -> () => a.sum + 1 }
    Param.repeatable[Int]("p") >>*? { case a -> () => a.sum + 1 }
    Param.repeatable[Int]("p") >> (_.sum + 1)

    "test" :/: Param[Int]

    "test" :&: Param[Int]("p")

    "test" :&: Param.repeatable[Int]("p")
  }

  {
    import syntax.PathTupleHandlerSyntax

    "test" >>* { param =>
      implicitly[param.type <:< Unit]
      22
    }
    "test" >>*? { case () => 22 }
  }

  {
    import syntax.StringHandlerSyntax
    "test" >> 22
  }

  {
    import syntax.PathSegmentSyntax
    "test" :/: "test"
  }
}
