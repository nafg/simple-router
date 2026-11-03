package simplerouter.derivation

import magnolia1.{CaseClass, Derivation, SealedTrait}
import simplerouter.Stringable

private[derivation] trait StringableDerivationMagnolia extends Derivation[Stringable] {
  override def join[T](caseClass: CaseClass[Stringable, T]): Stringable[T] =
    Encoding.product(
      caseClass.params.toSeq.map(p =>
        Encoding.Field[T](t => p.typeclass.format(p.deref(t)), text => p.typeclass.parse(text))
      )
    )(caseClass.rawConstruct)

  override def split[T](sealedTrait: SealedTrait[Stringable, T]): Stringable[T] =
    Encoding.sum(sealedTrait.subtypes.toSeq.map(s =>
      Encoding.Subtype[T](s.typeInfo.short, text => s.typeclass.parse(text))
    )) {
      value => sealedTrait.choose(value)(s => (s.typeInfo.short, s.typeclass.format(s.value)))
    }
}
