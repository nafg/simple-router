package simplerouter.derivation

import scala.language.experimental.macros
import scala.reflect.macros.whitebox

import magnolia1.{CaseClass, Magnolia, SealedTrait}
import simplerouter.Stringable

private[derivation] trait StringableDerivationMagnolia {
  type Typeclass[T] = StringablePart[T]

  def join[T](caseClass: CaseClass[StringablePart, T]): StringablePart[T] =
    new StringablePart(
      Encoding.product(
        caseClass.parameters.map(p =>
          Encoding.Field[T](
            t => p.typeclass.stringable.format(p.dereference(t)),
            text => p.typeclass.stringable.parse(text)
          )
        )
      )(caseClass.rawConstruct)
    )

  def split[T](sealedTrait: SealedTrait[StringablePart, T]): StringablePart[T] =
    new StringablePart(
      Encoding.sum(sealedTrait.subtypes.map(s =>
        Encoding.Subtype[T](s.typeName.short, text => s.typeclass.stringable.parse(text))
      )) { value =>
        sealedTrait.split(value)(s => (s.typeName.short, s.typeclass.stringable.format(s.cast(value))))
      }
    )

  def derived[T]: Stringable[T] = macro StringableDerivationMacros.derived[T]
}

private[derivation] object StringableDerivationMacros {
  def derived[T: c.WeakTypeTag](c: whitebox.Context): c.Tree = {
    import c.universe.*
    q"${Magnolia.gen[T](c)}.stringable"
  }
}
