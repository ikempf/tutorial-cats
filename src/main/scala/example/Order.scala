package example

case class Order(totalCost: Double, quantity: Double)

object Order {
  import cats.Monoid

  // TODO 03: define a monoid on Order
  implicit val orderMonoid = new Monoid[Order] {
    override def empty: Order = Order(0, 0)
    override def combine(x: Order, y: Order): Order =
      Order(x.totalCost + y.totalCost, x.quantity + y.quantity)
  }

}
