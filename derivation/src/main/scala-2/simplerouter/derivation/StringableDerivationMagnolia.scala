package simplerouter.derivation

import scala.language.experimental.macros

import magnolia1.{CaseClass, Magnolia, SealedTrait}
import simplerouter.Stringable

private[derivation] trait StringableDerivationMagnolia {
  type Typeclass[T] = Stringable[T]

  def join[T](caseClass: CaseClass[Stringable, T]): Stringable[T] =
    Encoding.product(
      caseClass.parameters.map(p => Encoding.Field[T](t => p.typeclass.format(p.dereference(t)), p.typeclass.parse))
    )(caseClass.rawConstruct)

  def split[T](sealedTrait: SealedTrait[Stringable, T]): Stringable[T] =
    Encoding.sum(sealedTrait.subtypes.map(s => Encoding.Subtype[T](s.typeName.short, s.typeclass.parse))) {
      value => sealedTrait.split(value)(s => (s.typeName.short, s.typeclass.format(s.cast(value))))
    }

  def derived[T]: Stringable[T] = macro Magnolia.gen[T]
}
