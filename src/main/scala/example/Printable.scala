package example

trait Printable[A] {
  def format(a: A): String

  // TODO 04: implement the contramap function
  // The 'contramap' allow to prepend a transformation before an operation
  // apply format to f
  def contramap[B](f: B => A): Printable[B] = (b: B) => format(f(b))
}

object PrintableInstances {

  // TODO 02: define a printable of 'Int'
  implicit val printableInt: Printable[Int] = (a: Int) => s"value=${Integer.toString(a)}"

  // TODO 02: define a printable of 'String'
  implicit val printableString: Printable[String] = (a: String) => s"value=$a"

  // TODO 04: define a printable of 'Boolean'
  implicit val printableBoolean: Printable[Boolean] = (b: Boolean) => b.toString

  // TODO 02: define a printable of 'Gato'
  implicit val printableCat: Printable[Gato] = (cat: Gato) => s"name=${cat.name}, age=${cat.age}, color=${cat.color}"

}

object Printable {
  // TODO 02: complete the format function to accept an implicit printable to format a
  def format[A](a: A)(implicit p: Printable[A]) = p.format(a)
}

object PrintableSyntax {
  // TODO 02: define a class PrintableOps[A] to add 'format' to 'A'
  implicit class PrintableOps[A](a: A) {
    def format(implicit p: Printable[A]) = p.format(a)
  }
}
