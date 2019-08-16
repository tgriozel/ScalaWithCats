package sandbox.eq

import cats.Eq
import sandbox.model.Cat

object EqInstances {
  implicit val catEq: Eq[Cat] = Eq.fromUniversalEquals[Cat]
}
