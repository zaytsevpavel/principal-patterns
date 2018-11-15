package com.pzaytsev
import scalaz._
import Scalaz._
object ReadersAndInjection extends App {

  case class Cat(name: String, favoriteFood: String)
  // wrap functions of one argument to compose
  val catName: Reader[Cat, String] = Reader(cat => cat.name)
  val garfield = Cat("Garfield", "lasagne")
  println(catName.run(garfield))
  // compose function within a reader
  val greeting = catName.map(name => s"Hello $name.")
  val heathcliff = Cat("Heathcliff", "junkfood")
  println(greeting.run(heathcliff))
  val feeding: Reader[Cat, String] = Reader(
    cat => s"Have a nice bowl of ${cat.favoriteFood}.")
  // combine Readers that take same arguments

  val greetingAndFeeding: Reader[Cat, String] = for {
    greet <- greeting
    feed <- feeding
  } yield greet + " " + feed

  println(greetingAndFeeding(garfield), greetingAndFeeding(heathcliff))

  case class Total(a: String, db: DB)
  case class DB(username: Map[Int, String], passwords: Map[String, String])
  type DBReader[A] = Reader[DB, A]

  def findUserName(userId: Int): DBReader[Option[String]] =
    Reader(dbConfig => dbConfig.username.get(userId))

  def checkPassword(username: String, password: String): DBReader[Boolean] =
    Reader { config =>
      config.passwords.get(username).contains(password)
    }

  def checkLogin(userId: Int, password: String): DBReader[Boolean] =
    for {
      user <- findUserName(userId)
      result <- user match {
        case Some(u) => checkPassword(u, password)
        case None    => false.pure[DBReader]
      }
    } yield result

  // reader is just a kleisli with an id type constructor
  val users = Map(1 -> "kevin", 2 -> "john")
  val passwords = Map("kevin" -> "010101", "george" -> "393933")
  val db = DB(users, passwords)
  println(checkLogin(1, "010101").run(db), checkLogin(2, "33333").run(db))
}
