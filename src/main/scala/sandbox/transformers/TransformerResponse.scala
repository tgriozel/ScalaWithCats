package sandbox.transformers

import cats.data.EitherT
import cats.implicits._

import java.util.concurrent.TimeUnit
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

object TransformerResponse {
  type Response[A] = EitherT[Future, String, A]

  private val SpecialMoveThreshold = 15

  private val CommTimeout = Duration.create(5, TimeUnit.SECONDS)

  private val PowerLevels = Map(
    "Jazz" -> 6,
    "Bumblebee" -> 8,
    "Hot Rod" -> 10
  )

  def getPowerLevel(autoBot: String): Response[Int] = {
    PowerLevels.get(autoBot) match {
      case Some(level) => EitherT.right(Future(level))
      case None => EitherT.left(Future(s"'$autoBot' unreachable"))
    }
  }

  def canSpecialMove(ally1: String, ally2: String): Response[Boolean] = {
     for {
       level1 <- getPowerLevel(ally1)
       level2 <- getPowerLevel(ally2)
     } yield level1 + level2 > SpecialMoveThreshold
  }

  def tacticalReport(ally1: String, ally2: String): String = {
    val stack = canSpecialMove(ally1, ally2).value
    Await.result(stack, CommTimeout) match {
      case Left(errorMsg) =>
        errorMsg
      case Right(true) =>
        s"$ally1 and $ally2 can perform special move"
      case Right(false) =>
        s"$ally1 and $ally2 do not have enough power"
    }
  }
}
