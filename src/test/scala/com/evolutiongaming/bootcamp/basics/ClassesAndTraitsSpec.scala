package com.evolutiongaming.bootcamp.basics

import ClassesAndTraits._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalactic.TolerantNumerics

class ClassesAndTraitsSpec extends AnyFlatSpec {

  // For comparing double values
  implicit val doubleEquality = TolerantNumerics.tolerantDoubleEquality(0.01)

  "Movable trait" should "be correct" in {
    val point = Point(1, 2)
    val circle = Circle(0, 0, 5)
    val rectangle = Rectangle(Point(9, -1), Point(13, 3))

    point.move(-3, 4) shouldEqual Point(-2, 6)
    circle.move(9, -8) shouldEqual circle.copy(centerX = 9, centerY = -8)
    rectangle.move(-2, -2) shouldEqual Rectangle(Point(7, -3), Point(11, 1))
  }

  behavior of "A rectangle"

  it should "correct calculate the center point" in {
    val rect1 = Rectangle(Point(9, -1), Point(13, 3))
    val rect2 = Rectangle(Point(2, 9), Point(10, 13))

    rect1.center shouldEqual Point(11, 1)
    rect2.center shouldEqual Point(6, 11)
  }

  it should "correct calculate lengths of sides given UL and BR points" in {
    val rect1 = Rectangle(Point(2, 9), Point(10, 13))
    val rect2 = Rectangle(Point(-3, -6), Point(3, 2))

    rect1.sides shouldEqual (4, 8)
    rect2.sides shouldEqual (8, 6)
  }

  it should "correct calculate its area" in {
    val rect1 = Rectangle(Point(-13, 1), Point(-8.5, 7))
    val rect2 = Rectangle(Point(-7, 10), Point(-3.75, 15))

    rect1.area === 27.0
    rect2.area === 21.25
  }

  it should "correct calculate its circuit" in {
    val rect1 = Rectangle(Point(-7, 10), Point(-3.75, 15))

    rect1.circuit === 18.5
  }

  behavior of "A square"

  it should "correct calculate its area" in {
    val sq1 = Square(Point(9, -1), 4)

    sq1.area === 16.0
  }

  behavior of "A triangle"

  it should "correct calculate its area" in {
    val tri1 = Triangle(Point(-7, -5), Point(0, -6), Point(4, -11))

    tri1.area === 15.46
  }

  behavior of "A sphere"

  it should "correct calculate its surface area" in {
    val sphere1 = Sphere(Point3D(1, 1, 1), 5)

    sphere1.surfaceArea === 314.0
  }

  it should "correct calculate its volume" in {
    val sphere1 = Sphere(Point3D(1, 1, 1), 5)

    sphere1.volume === 523.33
  }

  behavior of "A cube"

  it should "correct calculate its surface area" in {
    val cube1 = Cube(Square3D(Point3D(-15, -9, 3), 4))

    cube1.surfaceArea === 96.0
  }


  it should "correct calculate its volume" in {
    val cube1 = Cube(Square3D(Point3D(1, 1, 1), 4))

    cube1.volume === 64.0
  }

  behavior of "A cuboid"

  it should "correct calculate its surface area" in {
    val cuboid1 = Cuboid(Rectangle3D(Point3D(-15, -9, 3), Point3D(-10, -9, 1)), 15)

    cuboid1.surfaceArea === 230.0
  }

  it should "correct calculate its volume" in {
    val cuboid1 = Cuboid(Rectangle3D(Point3D(-15, -9, 3), Point3D(-10, -9, 1)), 15)

    cuboid1.volume === 150.0
  }
}
