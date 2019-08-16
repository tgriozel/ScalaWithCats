package sandbox.monoid

import cats.Monoid
import sandbox.model.Order

object MonoidInstances {
  implicit val booleanAndMonoid: Monoid[Boolean] = new Monoid[Boolean]{
    override def empty: Boolean = true
    override def combine(x: Boolean, y: Boolean): Boolean = x && y
  }
  implicit val booleanOrMonoid: Monoid[Boolean] = new Monoid[Boolean]{
    override def empty: Boolean = false
    override def combine(x: Boolean, y: Boolean): Boolean = x || y
  }
  implicit val orderMonoid: Monoid[Order] = new Monoid[Order]{
    override def empty: Order = Order(0, 0.0)
    override def combine(x: Order, y: Order): Order =
      Order((x.totalCost * x.quantity) + (y.totalCost * y.quantity), 1)
  }
}
