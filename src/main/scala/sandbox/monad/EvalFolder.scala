package sandbox.monad

import cats.Eval

class EvalFolder {

  def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B = foldRightEval(as, acc)(fn).value

  def foldRightEval[A, B](as: List[A], acc: B)(fn: (A, B) => B): Eval[B] = as match {
    case head :: tail =>
      Eval.defer {
        foldRightEval(tail, acc)(fn).map { tailEvaluated =>
          fn(head, tailEvaluated)
        }
      }
    case Nil =>
      Eval.now(acc)
  }

}
