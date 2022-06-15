package sandbox.monoid

import cats.Monoid

import scala.concurrent.{ExecutionContext, Future}

object FoldMap {

  def foldMap[A, B](xs: Vector[A])(f: A => B)(implicit m: Monoid[B]): B = {
    xs.map(f).foldLeft(m.empty)(m.combine)
  }

  private val safeCpuCount = {
    val runtimeCpuCount = Runtime.getRuntime.availableProcessors
    if (runtimeCpuCount <= 1) 1 else runtimeCpuCount
  }

  def parallelFoldMap[A, B](xs: Vector[A])(f: A => B)(implicit m: Monoid[B], ec: ExecutionContext): Future[B] = {
    val groups = xs.grouped(xs.size / safeCpuCount)
    val foldMappedGroups = groups.map(g => Future(foldMap(g)(f)))
    Future.sequence(foldMappedGroups).map(_.foldLeft(m.empty)(m.combine))
  }

}
