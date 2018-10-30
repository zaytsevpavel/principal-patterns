package com.pzaytsev

import cats.Eval

// val - Now - eager, memoized (don't recompute on invocation)
// defs - Always - lazy, non-memoized
// lazy val - Later - lazy, memoized

object EvaluationModelsAbstraction extends App {
  val now = Eval.now(math.random + 1000)

  val later = Eval.later(math.random + 2000)

  val always = Eval.always(math.random + 3000)

  val a = Eval
    .always { println("Hello this is "); "a cat" }
    .map { str =>
      println("me "); s"$str sat on the"
    }
    .memoize // everything before is cached, everything after retains an original semantics (non-memoized, lazy)
    .map { str =>
      println(" Mario"); s"$str banch"
    }

  println(a.value, a.value)
  def factorial(n: BigInt): Eval[BigInt] =
    if (n == 1) {
      Eval.now(n)
    } else {
      Eval.defer(factorial(n - 1).map(_ * n)) // trampolining recursive calls - if not Eval.defer we perform recursive calls before calling Eval's map - we might run out of stack
    }

  println(factorial(5000).value)

  // stack-safe trampolined fold-right:

  def foldRightEval[A, B](as: List[A], acc: B)(fn: (A, B) => B): Eval[B] = {
    as match {
      case head :: tail =>
        Eval.defer(
          foldRightEval(tail, acc)(fn).map(foldResult => fn(head, foldResult)))
      case Nil => Eval.now(acc)
    }
  }

  val longList: List[BigInt] = List.fill(1000000)(5)
  println(foldRightEval(longList, BigInt(0))((x, y) => x + y).value)

}
