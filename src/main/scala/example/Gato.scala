package example

final case class Gato(name: String, age: Int, color: String)

object Gato {
  import cats.Eq
  import cats.syntax.eq._
  import cats.instances.string._
  import cats.instances.int._

  // TODO 01: define equality for Gato
  // could/should be defined using Eq[Int] and Eq[String], but lazy
  implicit val catEquality = new Eq[Gato] {
    override def eqv(x: Gato, y: Gato): Boolean = x == y
  }

  // This function is here is only to avoid the clash with scalatest!
  // TODO 01: use the function '===' to compare cats
  def isEqual(first: Gato, second: Gato) =
    first === second
}
