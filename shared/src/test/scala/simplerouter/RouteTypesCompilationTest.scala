package simplerouter

import simplerouter.syntax.*

/** Compiles only if the routes built by `>>` have the value type of their path. On Scala 3, a type inherited from the
  * DSL's traits can keep the trait's `this` as its prefix, which only shows when the library is compiled separately
  * from the code using it, as these tests are.
  */
object RouteTypesCompilationTest {
  val param: Route[Int ->: Unit, Int]                                 = Param[Int] >> (i => i)
  val query: Route[Option[Int] ->: Unit, Int]                         = Param[Int]("q") >> (_.sum)
  val repeated: Route[List[Int] ->: Unit, Int]                        = Param.repeatable[Int]("q") >> (_.sum)
  val path0: Route[Unit, Int]                                         = Path("a") >> 1
  val path1: Route[Int ->: Unit, Int]                                 = "a" :/: Param[Int] >> (i => i)
  val path2: Route[Int ->: Int ->: Unit, Int]                         = Param[Int] :/: Param[Int] >> (a => b => a + b)
  val path3: Route[Int ->: Int ->: Int ->: Unit, Int]                 =
    Param[Int] :/: Param[Int] :/: Param[Int] >> (a => b => c => a + b + c)
  val path4: Route[Int ->: Int ->: Int ->: Int ->: Unit, Int]         =
    Param[Int] :/: Param[Int] :/: Param[Int] :/: Param[Int] >> (a => b => c => d => a + b + c + d)
  val path5: Route[Int ->: Int ->: Int ->: Int ->: Int ->: Unit, Int] =
    Param[Int] :/: Param[Int] :/: Param[Int] :/: Param[Int] :/: Param[Int] >>
      (a => b => c => d => e => a + b + c + d + e)
  val string: Route[Unit, Int]                                        = "a" >> 1
  val constructed: Location                                           = path1.path.construct(1)
}
