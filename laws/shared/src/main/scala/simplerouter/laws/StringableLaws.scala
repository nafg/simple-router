package simplerouter.laws

import org.scalacheck.Prop.*
import org.scalacheck.util.Pretty
import org.scalacheck.{Arbitrary, Prop, Shrink}
import simplerouter.Stringable

/** ScalaCheck properties that every `Stringable` must satisfy */
object StringableLaws {

  /** Holds when `s` parses the text it formats for each generated value back to that value
    *
    * @param s
    *   the instance under test
    * @param shrink
    *   shrinks a failing value
    * @param pretty
    *   renders a failing value in the report
    */
  def roundTrip[A: Arbitrary](s: Stringable[A])(implicit shrink: Shrink[A], pretty: A => Pretty): Prop =
    forAll { (a: A) =>
      val text = s.format(a)
      s.parse(text).fold(falsified :| s"couldn't parse '$text'")(parsed => (parsed ?= a) :| s"parsed '$text'")
    }
}
