package simplerouter

import java.time.Instant

import org.scalacheck.Properties
import simplerouter.laws.StringableLaws.roundTrip

object StringablePropertiesJVM extends Properties("Stringable") {
  import StringableProperties.arbInstant

  property("instant") = roundTrip(implicitly[Stringable[Instant]])
}
