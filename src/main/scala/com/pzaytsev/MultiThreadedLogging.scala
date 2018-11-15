package com.pzaytsev

import cats.data.Chain.{Append, Singleton}
import cats.data.{Chain, Writer}
import cats.instances.vector._
import cats.syntax.applicative._
import cats.syntax.writer._

object MultiThreadedLogging extends App {
  Writer(Vector("It was a best of times", "It was the end of times"), 1859)

  // a result and no log:
  type Logged[A] = Writer[Vector[String], A]
  type LoggedChain[A] = Writer[Chain[String], A]

  println(123.pure[Logged])

  // a log and no result:
  println(Vector("msg1", "msg2", "msg3").tell)

  // both:
  println(123.writer(Vector("msg1", "msg2", "msg3")))

  // extract:
  val writer = (200 + 300).writer(Vector("blah", "blah"))

  println(writer.value)
  println(writer.written)
  println(writer.run)

  // more efficient concatenation:

  val writerChained = (500 + 600).writer(Chain.fromSeq(Vector("a", "b", "c")))
  println(writerChained.run)

  val writerTransformed = for {
    a <- 10.pure[LoggedChain]
    _ <- Chain.fromSeq(Vector("x", "y", "z")).tell
    b <- 32.writer(Chain.fromSeq(Vector("a", "b", "c")))

  } yield a + b

  println(writerTransformed.run)

  // map a log
  println(writerTransformed.mapWritten(_.map(_.toUpperCase)).run)
}
