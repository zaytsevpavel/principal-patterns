package com.pzaytsev

// How polymorphism works in OOP vs Pattern Matching vs Type Classes

abstract class ShapeObject {
  def getArea(): Double
}

class RectangleObject(width: Int, length: Int) extends ShapeObject {
  override def getArea(): Double = width * length
}

class CircleObject(radius: Int) extends ShapeObject {
  override def getArea(): Double = radius * radius * Math.PI
}

sealed trait ShapePattern // exhaustive but also can be not - what is something is not shape but has an area?
case class RectanglePattern(width: Int, length: Int) extends ShapePattern
case class CirclePattern(radius: Int) extends ShapePattern

object ShapePattern {
  def getArea(shape: ShapePattern): Double = shape match {
    case RectanglePattern(width, length) => width * length
    case CirclePattern(radius)           => radius * radius * Math.PI
  }
}

// type class has 4 components:

// 1. a shape type class itself

trait ShapeLike[A] {
  def getArea(shape: A): Double
}

// 2. type class instances (defined in the scope of the respective companion objects):

object RectanglePattern {

  implicit object rectangleArea extends ShapeLike[RectanglePattern] {
    override def getArea(shape: RectanglePattern): Double =
      shape.length * shape.width
  }
}

object CirclePattern {
  implicit object circleArea extends ShapeLike[CirclePattern] {
    override def getArea(shape: CirclePattern): Double =
      shape.radius * shape.radius * Math.PI
  }
}

// 3. interface that we call. it uses the implicit parameters:

object ShapeProvider {
  def areafi[A](shapeLikeThingy: A)(
      implicit shapeAreaMaker: ShapeLike[A]): Double = {
    shapeAreaMaker.getArea(shapeLikeThingy)
  }

  // usually interface of a type class instance is the same interface we want to use, then just return this instance.
  def apply[A](implicit shapeAreaMaker: ShapeLike[A]): ShapeLike[A] =
    shapeAreaMaker
}

// 4. enrichment interface - its as if method is defined on the class itself (optional but still used a lot)
// create an implicit class and define a method that takes in a type class instance implicitly.
// Your type will be passed in the class implicitly, after which, we will call a method on it.

object SyntaxForThisApp {
  implicit class RectangleSyntax[A](shapeLikeThingy: A) {
    def getThisShape(implicit shapeAreaMaker: ShapeLike[A]): Double =
      shapeAreaMaker.getArea(shapeLikeThingy)
  }
}

object Polymorphism extends App {

  // Java world (dinosaurs):
  def getShapeOOP(shape: ShapeObject): Double = shape.getArea()

  println(getShapeOOP(new CircleObject(2)))
  println(getShapeOOP(new RectangleObject(2, 5)))
  // Scala world for n00bs:
  def getShapePattern(shape: ShapePattern): Double = ShapePattern.getArea(shape)

  println(getShapePattern(CirclePattern(2)))
  println(getShapePattern(RectanglePattern(2, 5)))
  // Ad-hoc polymorphism for the true haskelly principal edgy bois:
  import SyntaxForThisApp._

  // with interface
  println(ShapeProvider[CirclePattern].getArea(CirclePattern(2)),
          ShapeProvider[RectanglePattern].getArea(RectanglePattern(2, 5)))

  // with implicit syntax
  println(CirclePattern(2).getThisShape)
  println(RectanglePattern(2, 5).getThisShape)

  // Now let's say we have a line, and it's technically not a shape, since it's one dimensional
  // but we need to compute it's area.

  // We can't use OOP for sure, since inheritance won't really make sense. In that case, we would have to define a shape
  // as an interface to begin with and make classes implement an interface.

  // Pattern matching will work, but will not be exhaustive as Line is not part of an ADT.
  // So we lose compile-time type safety.

  // With type classes:
  case class Line(length: Int)

  object Line {
    implicit object lineArea extends ShapeLike[Line] {
      override def getArea(shape: Line): Double = shape.length
    }
  }

  println(Line(13).getThisShape)

  // to utilize a first-class type class syntax support use https://github.com/mpilquist/simulacrum

}
