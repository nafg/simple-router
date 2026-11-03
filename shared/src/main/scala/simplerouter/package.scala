/** A typesafe, reversible router. A [[simplerouter.Path Path]] describes URLs, both to match them and to build them;
  * pairing paths with handlers gives [[simplerouter.Routes Routes]], which turn a [[simplerouter.Location Location]]
  * into a value.
  *
  * {{{
  * import simplerouter._
  * import simplerouter.syntax._
  *
  * val userPosts = "users" :/: Param[Long] :/: "posts" :&: Param[Int]("page")
  * val routes    = userPosts >> { id => page => (id, page) }
  *
  * userPosts.construct(42, Some(2)).toUrl                                  // "/users/42/posts?page=2"
  * routes.run(Location(List("users", "42", "posts"), List("page" -> "3"))) // (42, Some(3))
  * }}}
  */
package object simplerouter {
  @deprecated("Use Unit", "0.2.0")
  type RConst = Unit

  @deprecated("Use ->:", "0.2.0")
  type >>:[A, B] = (A, B)
}
