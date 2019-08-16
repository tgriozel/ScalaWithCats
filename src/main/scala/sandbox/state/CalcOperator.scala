package sandbox.state

import cats.data.State
import cats.implicits._

object CalcOperator {
  private type CalcState[A] = State[List[Int], A]

  private case class TwoElementsAndList[A](elem1: A, elem2: A, list: List[A])

  private def popTwoOrGTFO[A](stack: List[A]): TwoElementsAndList[A] = stack match {
    case  a :: b :: tail => TwoElementsAndList(a, b, tail)
    case _ => throw new IllegalArgumentException("could not find 2 elements")
  }

  private def applyOpOnStack[A](op: (A, A) => A)(stack: List[A]): (List[A], A) = {
    val TwoElementsAndList(a, b, reducedStack) = popTwoOrGTFO(stack)
    val result = op(b, a)
    (result +: reducedStack, result)
  }

  private def evalOne(char: Char): CalcState[Int] = char.asDigit match {
    case number if number >= 0 && number <= 9 =>
      State[List[Int], Int] { oldStack =>
        (number +: oldStack, number)
      }
    case _ =>
      char match {
        case '+' =>
          State[List[Int], Int](applyOpOnStack(_ + _))
        case '-' =>
          State[List[Int], Int](applyOpOnStack(_ - _))
        case '*' =>
          State[List[Int], Int](applyOpOnStack(_ * _))
        case '/' =>
          State[List[Int], Int](applyOpOnStack(_ / _))
        case other =>
          throw new IllegalArgumentException(s"$other is not a supported symbol")
      }
  }

  private def evalAll(chars: List[Char]): CalcState[Int] =
    chars.foldLeft(0.pure[CalcState]) { (acc, char) =>
      acc.flatMap(_ => evalOne(char))
    }

  def evalPostFix(input: String): Int =
    evalAll(input.toCharArray.toList).runEmptyA.value

}
