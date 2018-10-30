package com.pzaytsev
import cats._
import cats.syntax.either._
sealed trait LoginError extends Product with Serializable
case class UserNotFoundError(str: String) extends LoginError
case class PasswordNotFoundError(str: String) extends LoginError
case class User(username: String, password: String)

// Easier to define the domain error that can result from a certain functionality.
// Works for bifunctors like Either or Scalaz IO

// If your error domain is for sure mutually exclusive - use either
// If your errors may overlap - use Validated data type

object DomainErrors extends App {
  type LoginResult = Either[LoginError, User]

  def handleError(error: LoginError): Unit = error match {
    case UserNotFoundError(u)       => println(s"User $u not found")
    case PasswordNotFoundError(pwd) => println(s"Incorrect password: $pwd")
  }

  def login(result: LoginResult): Unit = {
    result.fold(handleError, println)
  }

  val user: LoginResult = User("name", "password").asRight[LoginError]
  val error: LoginResult = UserNotFoundError("non-existent").asLeft[User]
  login(user)
  login(error)

}
