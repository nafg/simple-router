package simplerouter.derivation

import scala.deriving.Mirror

import magnolia1.{CaseClass, Derivation, SealedTrait}
import simplerouter.Stringable

private[derivation] trait StringableDerivationMagnolia {
  inline def derived[A](using Mirror.Of[A]): Stringable[A] = StringablePartDerivation.derived[A].stringable
}

private[derivation] object StringablePartDerivation extends Derivation[StringablePart] {
  override def join[T](caseClass: CaseClass[StringablePart, T]): StringablePart[T] =
    new StringablePart(
      Encoding.product(
        caseClass.params.toSeq.map(p =>
          Encoding.Field[T](t => p.typeclass.stringable.format(p.deref(t)), text => p.typeclass.stringable.parse(text))
        )
      )(caseClass.rawConstruct)
    )

  override def split[T](sealedTrait: SealedTrait[StringablePart, T]): StringablePart[T] =
    new StringablePart(
      Encoding.sum(sealedTrait.subtypes.toSeq.map(s =>
        Encoding.Subtype[T](s.typeInfo.short, text => s.typeclass.stringable.parse(text))
      )) { value =>
        sealedTrait.choose(value)(s => (s.typeInfo.short, s.typeclass.stringable.format(s.value)))
      }
    )
}
