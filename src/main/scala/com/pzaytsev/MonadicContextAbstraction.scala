package com.pzaytsev
import cats.Id
import cats.Monad
import cats.syntax.functor._
import cats.syntax.flatMap._
import scala.language.higherKinds
import cats.instances.option._
import cats.instances.list._
import cats.instances.either._
import cats.syntax.either._

// abstracting over monadic context - a part of finally tagless encoding

object MonadicContextAbstraction extends App {
  def sumSquare[F[_]: Monad](a: F[Int], b: F[Int]): F[Int] = {
    for {
      inA <- a
      inB <- b
    } yield inA * inA + inB * inB
  }

  def countPositive(nums: List[Int]): Either[String, Int] = {
    nums.foldLeft(0.asRight[String]) { (acc, num) =>
      if (num > 0) acc.map(_ + 1) else Left("Negative")
    }
  }

  println(sumSquare(Option(3), Option(4)))
  println(sumSquare(List(1, 2, 3), List(4, 5, 6)))
  // Scala cannot unify types and type constructors when searching for implicits, so you need to cast
  println(sumSquare(2: Id[Int], 3: Id[Int]))
  val a = 1.asRight[String] // smart constructor returning a more general type
  val b = 2.asRight[String]
  println(sumSquare(a, b))
  println(countPositive(List(2, 3, 5)), countPositive(List(1, 2, 3, -1)))
}
