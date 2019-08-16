package sandbox.monoid

import cats.Monoid
import cats.syntax.semigroup._

object SuperAdder {
  def add[A : Monoid](items: List[A]): A = {
    items.reduceOption((a, b) => a |+| b).getOrElse(Monoid[A].empty)
  }
}
