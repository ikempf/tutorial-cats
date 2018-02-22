package example

import cats.Foldable

object Calculator {

  import cats.Monoid

  /*
   * TODO 03: implement the generic 'add' on any monoid
   * Note:
   * - accept a List of T
   * - accept an implicit monoid of T
   * - fold over the list
   * - combine the different T using |+|
   */
  def add[T](items: List[T])(implicit m: Monoid[T]): T = {
    import cats.syntax.semigroup._
    import cats.instances.list._
    Foldable[List].fold(items)
    // or
    items.foldLeft(m.empty)(_ |+| _)
  }
}
