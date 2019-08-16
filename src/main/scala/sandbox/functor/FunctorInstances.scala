package sandbox.functor

import cats.Functor
import sandbox.model.{Branch, Leaf, Tree}

object FunctorInstances {
  implicit val treeFunctor: Functor[Tree] =
    new Functor[Tree] {
      override def map[A, B](tree: Tree[A])(func: A => B): Tree[B] = tree match {
        case Leaf(value) =>
          Leaf(func(value))
        case Branch(left, right) =>
          val l = map(left)(func)
          val r = map(right)(func)
          Branch(l, r)
      }
    }
}
