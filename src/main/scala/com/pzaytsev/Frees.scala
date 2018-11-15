package com.pzaytsev

object Frees extends App {
  import scala.language.higherKinds

  sealed trait Interact[A]
  case class Tell(msg: String) extends Interact[Unit]
  case class Ask(prompt: String) extends Interact[String]

  val prog = List(Ask("Name?"), Ask("Last name?"), Tell("Hello ?")) // can't share data

  // turn data type into a monad

  trait Monad[F[_]] {
    def pure[A](a: A): F[A] // lift to context
    def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B] // bind
  }

  // bake control flow into a data type

  sealed trait Free[A] {
    def flatMap[B](f: A => Free[B]): Free[B] = {
      this match {
        case Return(a)            => f(a)
        case Bind(instance, func) => Bind(instance, func andThen (_ flatMap f))
      }
    }

    def map[B](f: A => B): Free[B] = flatMap(a => Return(f(a)))
  }

  case class Return[A](a: A) extends Free[A]
  case class Bind[A, B](instance: Free[A], f: A => Free[B]) extends Free[B]

  // now we need to turn Interact[A] to Free[A]

  // we need to use higher kinds (free should be typed by the type constructor and its data type

  sealed trait FreeMonad[F[_], A] {
    def flatMap[B](f: A => FreeMonad[F, B]): FreeMonad[F, B] = {
      this match {
        case ReturnFreeMonad(a) => f(a)
        case BindFreeMonad(instance, func) =>
          BindFreeMonad(instance, func andThen (_ flatMap f))
      }
    }

    def map[B](f: A => B): FreeMonad[F, B] = flatMap(a => ReturnFreeMonad(f(a)))

    // look below -- equivalent to run

    // g should be a monad
    def foldMap[G[_]](f: F ~> G)(implicit monad: Monad[G]): G[A] =
      this match {
        case ReturnFreeMonad(a) => monad.pure(a)
        case BindFreeMonad(i, k) =>
          monad.flatMap(f(i))(a => k(a).foldMap(f))
      }
  }

  case class ReturnFreeMonad[F[_], A](a: A) extends FreeMonad[F, A]
  case class BindFreeMonad[F[_], A, B](instance: F[A], f: A => FreeMonad[F, B])
      extends FreeMonad[F, B]

  implicit def liftIntoFree[F[_], A](fa: F[A]): FreeMonad[F, A] =
    BindFreeMonad(fa, (a: A) => ReturnFreeMonad(a))

  val prog2: FreeMonad[Interact, Unit] = for {
    firstname <- Ask("What's your firstname?")
    surname <- Ask("What's your surname?")
    _ <- Tell(s"Hello, $firstname $surname")
  } yield ()

  val expandedProg: FreeMonad[Interact, Unit] =
    Ask("What's your firstname?").flatMap(firstname =>
      Ask("What's your surname?").flatMap(surname =>
        Tell(s"Hello, $firstname $surname").map(_ => Return(()))))

  // created a monad out of thin air <-> created a dsl
  // a data structure that holds a computation without executing it

  // we need to turn interact into action

  trait ~>[F[_], G[_]] {
    def apply[A](fa: F[A]): G[A]
  }

  // first interpreter uses console to run a program
  type Id[A] = A

  object Console extends (Interact ~> Id) {
    def apply[A](i: Interact[A]): Id[A] =
      i match {
        case Tell(message) => println(message)
        case Ask(prompt) =>
          println(prompt)
          scala.io.StdIn.readLine()
      }
  }

  implicit val idMonad: Monad[Id] = new Monad[Id] {
    def pure[A](a: A): Id[A] = a
    def flatMap[A, B](a: Id[A])(f: A => Id[B]) = f(a)
  }

  prog2.foldMap(Console)

  // we need a function that applies interpreter (foldMap)

}
