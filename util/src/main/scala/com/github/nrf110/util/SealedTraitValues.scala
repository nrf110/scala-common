package com.github.nrf110.util

import language.experimental.macros
import scala.reflect.macros.blackbox

/**
 * Example usages:
 * {{{
 *   sealed trait ImageSize
 *   case object Small extends ImageSize
 *   case object Medium extends ImageSize
 *   case object Large extends ImageSize
 *
 *   scala> SealedTraitValues.values[ImageSize]
 *   res0: Set[ImageSize] = Set(Small, Medium, Large)
 * }}}
 * Another example of enumeration-like approach with sealed traits:
 * {{{
 *   sealed trait ImageSize
 *   object ImageSize {
 *     case object Small extends ImageSize
 *     case object Medium extends ImageSize
 *     case object Large extends ImageSize
 *     val values = SealedTraitValues.values[ImageSize]
 *   }
 * }}}
 * Note how the `values` is defined after the case objects.
 *
 *
 * Finally, credit goes to : http://stackoverflow.com/questions/13671734/iteration-over-a-sealed-trait-in-scala
 */
object SealedTraitValues {
  def values[A]: Set[A] = macro values_impl[A]

  def values_impl[A: c.WeakTypeTag](c: blackbox.Context) = {
    import c.universe._

    val symbol = weakTypeOf[A].typeSymbol

    def isSubtype(candidate: Symbol): Boolean = {
      candidate == symbol || candidate.typeSignature.baseClasses.drop(1).exists(isSubtype)
    }

    if (!symbol.isClass) c.abort(
      c.enclosingPosition,
      "Can only enumerate values of a sealed trait or class."
    ) else if (!symbol.asClass.isSealed) c.abort(
      c.enclosingPosition,
      "Can only enumerate values of a sealed trait or class."
    ) else {
      // check all sibling declarations for subtypes
      val siblingSubclasses: List[Symbol] = scala.util.Try {
        c.internal.enclosingOwner.owner.typeSignature.decls
          .filter(a => a.isClass || a.isModule || a.isModuleClass)
          .filter(isSubtype).toList
      } getOrElse Nil

      // create a list of all known subtypes
      val subclasses = symbol.asClass.knownDirectSubclasses.toList ::: siblingSubclasses

      // filter traits and abstract classes out of the candidates list
      val candidates = subclasses.filterNot(a => a.isClass && (a.asClass.isTrait || a.asClass.isAbstract))

      if (!candidates.forall(x => x.isModuleClass || x.isModule)) c.abort(
        c.enclosingPosition,
        "All children must be objects."
      ) else c.Expr[Set[A]] {
        def sourceModuleRef(sym: Symbol) = Ident(
          if (sym.isModule) sym else
            sym.asInstanceOf[
              scala.reflect.internal.Symbols#Symbol
              ].sourceModule.asInstanceOf[Symbol]
        )

        Apply(
          Select(
            reify(Set).tree,
            TermName("apply")
          ),
          candidates.map(sourceModuleRef(_))
        )
      }
    }
  }
}
