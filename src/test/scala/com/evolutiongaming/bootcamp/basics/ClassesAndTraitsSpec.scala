package com.evolutiongaming.bootcamp.basics

import ClassesAndTraits._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalactic.TolerantNumerics

class ClassesAndTraitsSpec extends AnyFlatSpec {

  // For comparing double values
  implicit val doubleEquality = TolerantNumerics.tolerantDoubleEquality(0.01)

  "Movable trait" should "be correct" in {
    val point = Point2D(1, 2)
    val circleOpt = Circle2D.of(0, 0, 5)
    val rectangle = Rectangle2D.of(Point2D(9, -1), Point2D(13, 3))

    point.move(-3, 4) shouldEqual Point2D(-2, 6)
    //circleOpt.get.move(9, -8) shouldEqual Circle2D.of(9, -8, 5)
    //rectangle.get.move(-2, -2) shouldEqual Rectangle2D.of(Point2D(7, -3), Point2D(11, 1))
  }

  behavior of "A rectangle"

  it should "correct calculate the center point" in {
    val rectOpt1 = Rectangle2D.of(Point2D(9, -1), Point2D(13, 3))
    val rectOpt2 = Rectangle2D.of(Point2D(2, 9), Point2D(10, 13))

    rectOpt1.get.center shouldEqual Point2D(11, 1)
    rectOpt2.get.center shouldEqual Point2D(6, 11)
  }

  it should "correct calculate lengths of sides given UL and BR points" in {
    val rectOpt1 = Rectangle2D.of(Point2D(2, 9), Point2D(10, 13))
    val rectOpt2 = Rectangle2D.of(Point2D(-3, -6), Point2D(3, 2))

    rectOpt1.get.sides shouldEqual (4, 8)
    rectOpt2.get.sides shouldEqual (8, 6)
  }

  it should "correct calculate its area" in {
    val rectOpt1 = Rectangle2D.of(Point2D(-13, 1), Point2D(-8.5, 7))
    val rectOpt2 = Rectangle2D.of(Point2D(-7, 10), Point2D(-3.75, 15))

    rectOpt1.get.area === 27.0
    rectOpt2.get.area === 21.25
  }

  it should "correct calculate its circuit" in {
    val rectOpt1 = Rectangle2D.of(Point2D(-7, 10), Point2D(-3.75, 15))

    rectOpt1.get.circuit === 18.5
  }

  behavior of "A square"

  it should "correct calculate its area" in {
    val sqOpt1 = Square2D.of(Point2D(9, -1), 4)

    sqOpt1.get.area === 16.0
  }

  behavior of "A triangle"

  it should "correct calculate its area" in {
    val tri1 = Triangle2D(Point2D(-7, -5), Point2D(0, -6), Point2D(4, -11))

    tri1.area === 15.46
  }

  behavior of "A sphere"

  it should "correct calculate its surface area" in {
    val sphereOpt1 = Sphere.of(Point3D(1, 1, 1), 5)

    sphereOpt1.get.surfaceArea === 314.0
  }

  it should "correct calculate its volume" in {
    val sphereOpt1 = Sphere.of(Point3D(1, 1, 1), 5)

    sphereOpt1.get.volume === 523.33
  }

  behavior of "A cube"

  it should "correct calculate its surface area" in {
    val cubeOpt1 = Cube.of(Square3D.of(Point3D(-15, -9, 3), 4).get)

    cubeOpt1.get.surfaceArea === 96.0
  }


  it should "correct calculate its volume" in {
    val cubeOpt1 = Cube.of(Square3D.of(Point3D(1, 1, 1), 4).get)

    cubeOpt1.get.volume === 64.0
  }

  behavior of "A cuboid"

  it should "correct calculate its surface area" in {
    val cuboidOpt1 = Cuboid.of(Rectangle3D.of(Point3D(-15, -9, 3), Point3D(-10, -9, 1)).get, 15)

    cuboidOpt1.get.surfaceArea === 230.0
  }

  it should "correct calculate its volume" in {
    val cuboidOpt1 = Cuboid.of(Rectangle3D.of(Point3D(-15, -9, 3), Point3D(-10, -9, 1)).get, 15)

    cuboidOpt1.get.volume === 150.0
  }
}
