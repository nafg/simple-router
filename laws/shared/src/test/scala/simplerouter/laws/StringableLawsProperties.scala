package simplerouter.laws

import org.scalacheck.{Prop, Properties, Test}
import simplerouter.Stringable

object StringableLawsProperties extends Properties("StringableLaws") {
  private def fails(prop: Prop) = !Test.check(Test.Parameters.default, prop).passed

  property("roundTrip fails for an instance that can't parse its own output") =
    fails(StringableLaws.roundTrip(Stringable[Int](_ => None)(_.toString)))

  property("roundTrip fails for an instance that parses its output to a different value") =
    fails(StringableLaws.roundTrip(Stringable.int.xmap(_ + 1)(identity)))
}
