package com.evolutiongaming.bootcamp.basics

object ClassesAndTraits {

  // Object for basic operations
  object Utils {

    /** Approximate equals for double values */
    def ~= (x: Double, y: Double, precision: Double = 1e-2): Boolean =
      if ((x - y).abs < precision) true else false
  }

  sealed trait Located {
    def x: Double
    def y: Double
  }
  object Located {
    def distance(p1: Point, p2: Point): Double = math.sqrt(math.pow(p1.x - p2.x, 2) + math.pow(p1.y - p2.y, 2))
  }

  sealed trait Bounded {
    def minX: Double
    def maxX: Double
    def minY: Double
    def maxY: Double
  }

  // Here not every shape is movable
  sealed trait Shape extends Located with Bounded
  sealed trait Movable {
    def move(dx: Double, dy: Double): Shape
  }

  final case class Point(x: Double, y: Double) extends Shape with Movable {
    def minX: Double = x
    def maxX: Double = x
    def minY: Double = y
    def maxY: Double = y
    def move(dx: Double, dy: Double): Point = Point(x + dx, y + dy)
  }

  final case class Circle(centerX: Double, centerY: Double, radius: Double) extends Shape with Movable {
    def x: Double = centerX
    def y: Double = centerY
    def minX: Double = centerX - radius
    def maxX: Double = centerX + radius
    def minY: Double = centerY - radius
    def maxY: Double = centerY + radius
    def area: Double = math.Pi * math.pow(radius, 2)
    def circuit: Double = 2 * math.Pi * radius
    def move(dx: Double, dy: Double): Circle = Circle(x + dx, y + dy, radius)
  }

  /** A rectangle location is the center point */
  final case class Rectangle(UL: Point, BR: Point) extends Shape with Movable {
    require(UL.x < BR.x && UL.y < BR.y, "The UL point is lower or/and to the right of the BR point")
    def x: Double = center.x
    def y: Double = center.y
    def minX: Double = UL.x
    def maxX: Double = BR.x
    def minY: Double = UL.y
    def maxY: Double = BR.y
    def center: Point = Point((minX + maxX) / 2.0, (minY + maxY) / 2.0)
    def sides: (Double, Double) = (BR.y - UL.y, BR.x - UL.x)
    def area: Double = sides._1 * sides._2
    def circuit: Double = 2 * sides._1 + 2 * sides._2
    def move(dx: Double, dy: Double): Rectangle = Rectangle(UL.move(dx, dy), BR.move(dx, dy))
  }

  /** A square location is the center point */
  final case class Square(UL: Point, sideLength: Double) extends Shape {
    def BR: Point = Point(UL.x + sideLength, UL.y + sideLength)
    def x: Double = center.x
    def y: Double = center.y
    def minX: Double = UL.x
    def maxX: Double = BR.x
    def minY: Double = UL.y
    def maxY: Double = BR.y
    def center: Point = Point((minX + maxX) / 2.0, (minY + maxY) / 2.0)
    def circuit: Double = 4 * sideLength
    def area: Double = math.pow(sideLength, 2)
  }

  /** A triangle location is the center point */
  final case class Triangle(p1: Point, p2: Point, p3: Point) extends Shape {
    def x: Double = (p1.x + p2.x + p3.x) / 3.0
    def y: Double = (p1.y + p2.y + p3.y) / 3.0
    def minX: Double = (p1.x min p2.x) min p3.x
    def maxX: Double = (p1.x max p2.x) max p3.x
    def minY: Double = (p1.y min p2.y) min p3.y
    def maxY: Double = (p1.y max p2.y) max p3.y
    def sides: (Double, Double, Double) = (Located.distance(p1, p2), Located.distance(p2, p3), Located.distance(p3, p1))
    def circuit: Double = sides._1 + sides._2 + sides._3
    def area: Double = {
      val halfCircuit = circuit / 2.0
      math.sqrt(halfCircuit * (halfCircuit - sides._1) * (halfCircuit - sides._2) * (halfCircuit - sides._3))
    }
  }

  // The 3D world starts here

  sealed trait Located3D {
    def x: Double
    def y: Double
    def z: Double
  }
  object Located3D {
    def distance(p1: Point3D, p2: Point3D): Double = math.sqrt(math.pow(p1.x - p2.x, 2) + math.pow(p1.y - p2.y, 2) + math.pow(p1.z - p2.z, 2))
  }

  sealed trait Bounded3D {
    def minX: Double
    def maxX: Double
    def minY: Double
    def maxY: Double
    def minZ: Double
    def maxZ: Double
  }

  sealed trait Shape3D extends Located3D with Bounded3D

  object Origin3D extends Located3D {
    def x: Double = 0
    def y: Double = 0
    def z: Double = 0
  }

  final case class Point3D(x: Double, y: Double, z: Double) extends Shape3D {
    def minX: Double = x
    def maxX: Double = x
    def minY: Double = y
    def maxY: Double = y
    def minZ: Double = z
    def maxZ: Double = z
  }

  /** A sphere location is the center point */
  final case class Sphere(center: Point3D, radius: Double) extends Shape3D {
    def x: Double = center.x
    def y: Double = center.y
    def z: Double = center.z
    def minX: Double = x - radius
    def maxX: Double = x + radius
    def minY: Double = y - radius
    def maxY: Double = y + radius
    def minZ: Double = z - radius
    def maxZ: Double = z + radius
    def surfaceArea: Double = 4 * math.Pi * math.pow(radius, 2)
    def volume: Double = 4 / 3.0 * math.Pi * math.pow(radius, 3)
  }

  /** A 3d square location is the center point */
  final case class Square3D(UL: Point3D, sideLength: Double) extends Shape3D {
    def BR: Point3D = Point3D(UL.x + sideLength, UL.y, UL.z - sideLength)
    def x: Double = center.x
    def y: Double = center.y
    def z: Double = center.z
    def minX: Double = UL.x
    def maxX: Double = BR.x
    def minY: Double = UL.y
    def maxY: Double = BR.y
    def minZ: Double = UL.z
    def maxZ: Double = UL.z
    def center: Point3D = Point3D((minX + maxX) / 2.0, (minY + maxY) / 2.0, (minZ + maxZ) / 2.0)
    def circuit: Double = 4 * sideLength
    def area: Double = math.pow(sideLength, 2)
  }

  /** A cube location is the center point of the bottom base */
  final case class Cube(bottomBase: Square3D) extends Shape3D {
    def x: Double = baseCenter.x
    def y: Double = baseCenter.y
    def z: Double = baseCenter.z
    private val halfSideLength = bottomBase.sideLength / 2.0
    def minX: Double = x - halfSideLength
    def maxX: Double = x + halfSideLength
    def minY: Double = y - 2 * halfSideLength
    def maxY: Double = y
    def minZ: Double = z - halfSideLength
    def maxZ: Double = z + halfSideLength
    def baseCenter: Point3D = bottomBase.center
    def surfaceArea: Double = 6 * bottomBase.area
    def volume: Double = math.pow(bottomBase.sideLength, 3)
  }

  /** A 3d rectangle location is the center point */
  final case class Rectangle3D(UL: Point3D, BR: Point3D) extends Shape3D {
    if (Utils.~=(UL.y, BR.y)) require(UL.x < BR.x && UL.z > BR.z) else require(UL.x < BR.x && UL.y < BR.y)
    def x: Double = center.x
    def y: Double = center.y
    def z: Double = center.z
    def minX: Double = UL.x
    def maxX: Double = BR.x
    def minY: Double = UL.y
    def maxY: Double = BR.y
    def minZ: Double = UL.z min BR.z
    def maxZ: Double = UL.z max BR.z
    def center: Point3D = Point3D((minX + maxX) / 2.0, (minY + maxY) / 2.0, (minZ + maxZ) / 2.0)
    def sides: (Double, Double) = if (Utils.~=(UL.y, BR.y)) (maxZ - minZ, maxX - minX) else (minY - maxY, maxX - minX)
    def circuit: Double = 2 * sides._1 + 2 * sides._2
    def area: Double = sides._1 * sides._2
  }

  /** A cuboid location is the center point of the bottom base */
  final case class Cuboid(bottomBase: Rectangle3D, height: Double) extends Shape3D {
    def x: Double = baseCenter.x
    def y: Double = baseCenter.y
    def z: Double = baseCenter.z
    def baseCenter: Point3D = bottomBase.center
    def minX: Double = bottomBase.UL.x
    def maxX: Double = bottomBase.BR.x
    def minY: Double = y + height
    def maxY: Double = y
    def minZ: Double = bottomBase.BR.z
    def maxZ: Double = bottomBase.UL.z
    def surfaceArea: Double = 2 * bottomBase.area + 2 * bottomBase.sides._1 * height + 2 * bottomBase.sides._2 * height
    def volume: Double = bottomBase.area * height
  }

  /** A 3d triangle location is the center point */
  final case class Triangle3D(p1: Point3D, p2: Point3D, p3: Point3D) extends Shape3D {
    def x: Double = (p1.x + p2.x + p3.x) / 3.0
    def y: Double = (p1.y + p2.y + p3.y) / 3.0
    def z: Double = (p1.z + p2.z + p3.z) / 3.0
    def minX: Double = (p1.x min p2.x) min p3.x
    def maxX: Double = (p1.x max p2.x) max p3.x
    def minY: Double = (p1.y min p2.y) min p3.y
    def maxY: Double = (p1.y max p2.y) max p3.y
    def minZ: Double = (p1.z min p2.z) min p3.z
    def maxZ: Double = (p1.z max p2.z) max p3.z
    def sides: (Double, Double, Double) = (Located3D.distance(p1, p2), Located3D.distance(p2, p3), Located3D.distance(p3, p1))
    def circuit: Double = sides._1 + sides._2 + sides._3
    def area: Double = {
      val halfCircuit = circuit / 2.0
      math.sqrt(halfCircuit * (halfCircuit - sides._1) * (halfCircuit - sides._2) * (halfCircuit - sides._3))
    }
  }

  /** A triangular pyramid location is the top point */
  final case class TriangularPyramid(base: Triangle3D, top: Point3D, height: Double) extends Shape3D {
    private val sideWall1 = Triangle3D(base.p1, top, base.p2)
    private val sideWall2 = Triangle3D(base.p2, top, base.p3)
    private val sideWall3 = Triangle3D(base.p3, top, base.p1)
    def x: Double = top.x
    def y: Double = top.y
    def z: Double = top.z
    def minX: Double = (sideWall1.minX min sideWall2.minX) min sideWall3.minX
    def maxX: Double = (sideWall1.maxX max sideWall2.maxX) max sideWall3.maxX
    def minY: Double = (sideWall1.minY min sideWall2.minY) min sideWall3.minY
    def maxY: Double = (sideWall1.maxY max sideWall2.maxY) max sideWall3.maxY
    def maxZ: Double = (sideWall1.maxZ max sideWall2.maxZ) max sideWall3.maxZ
    def minZ: Double = (sideWall1.minZ min sideWall2.minZ) min sideWall3.minZ
    def surfaceArea: Double = base.area + sideWall1.area + sideWall2.area + sideWall3.area
    def volume: Double = 1 / 3.0 * base.area * height
  }

}
