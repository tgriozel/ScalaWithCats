package sandbox.monad

import cats.data.Writer

object FactorialWriter {

  private def slowly[A](body: => A): A =
    try body finally Thread.sleep(100)

  def factorial(n: Int): Writer[Vector[String], Int] = n match {
    case 0 =>
      val ans = slowly(1)
      Writer(Vector(s"fact $n $ans"), ans)
    case n =>
      slowly(factorial(n - 1).mapBoth { (log, res) =>
        (log :+ s"fact $n $res", n * res)
      })
  }

}
