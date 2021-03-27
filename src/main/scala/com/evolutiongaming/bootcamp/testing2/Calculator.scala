package com.evolutiongaming.bootcamp.testing2

import com.evolutiongaming.bootcamp.testing2.Calculator._

import java.math.{MathContext, RoundingMode}
import scala.util.Try

/** Simple calculator with buttons.
 *
 * @param memory whatever is stored in the memory.
 * @param screen whatever you see on the screen.
 */
final case class Calculator(memory: BigDecimal = 0d, screen: Screen = Screen.of("0").get, operation: Option[Operation] = None) {

  def restart: Calculator = Calculator()

  def enterDigit(digit: Input.Digit): Calculator =
    this.copy(screen = Screen.of(s"${screen.toString}${digit.data}").get)

  def enterDecPoint: Calculator =
    if (screen.strNumber.last == '.')
      this
    else
      this.copy(screen = Screen.of(s"${screen.toString}${Input.DecPoint.data}").get)

  def plus: Calculator = {
    val mem = screen.optConvertToNumWithPoint

    Calculator(operation = Some(Operation.Plus), screen = Screen.of("0").get,
      memory = mem)
  }

  def minus: Calculator = {
    val mem = screen.optConvertToNumWithPoint

    Calculator(operation = Some(Operation.Minus), screen = Screen.of("0").get,
      memory = mem)
  }

  def multiply: Calculator = {
    val mem = screen.optConvertToNumWithPoint

    Calculator(operation = Some(Operation.Multiply), screen = Screen.of("0").get,
      memory = mem)
  }

  def divide: Calculator = {
    val mem = screen.optConvertToNumWithPoint

    Calculator(operation = Some(Operation.Divide), screen = Screen.of("0").get,
      memory = mem)
  }

  def calculate: Either[String, Calculator] = operation.fold[Either[String, Calculator]](Right(this)) {

    case Operation.Plus => for {
      res <- Try(memory + screen.getNumber).toEither.left.map(_ => "ERR - overflow after plus")
    } yield Calculator(screen = Screen.of(res.toString).get)

    case Operation.Minus =>
      val res = memory - screen.getNumber
      Right(Calculator(screen = Screen.of(res.toString).get))

    case Operation.Multiply => for {
      res <- Try(memory * screen.getNumber).toEither.left.map(_ => "ERR - overflow after multiply")
    } yield Calculator(screen = Screen.of(res.toString).get)

    case Operation.Divide => for {
      res <- Try((memory / screen.getNumber).setScale(10, BigDecimal.RoundingMode.HALF_UP)).toEither.left.map(_ => "ERR - division by 0")
    } yield Calculator(screen = Screen.of(res.toString).get)
  }
}
object Calculator {

  sealed trait Input[+A] {
    def data: A
    override def toString: String = data.toString
  }
  object Input {

    sealed trait Digit extends Input[Int]
    object Digit {
      case object Zero extends Digit {
        override def data: Int = 0
      }
      case object One extends Digit {
        override def data: Int = 1
      }
      case object Two extends Digit {
        override def data: Int = 2
      }
      case object Three extends Digit {
        override def data: Int = 3
      }
      case object Four extends Digit {
        override def data: Int = 4
      }
      case object Five extends Digit {
        override def data: Int = 5
      }
      case object Six extends Digit {
        override def data: Int = 6
      }
      case object Seven extends Digit {
        override def data: Int = 7
      }
      case object Eight extends Digit {
        override def data: Int = 8
      }
      case object Nine extends Digit {
        override def data: Int = 9
      }
    }

    object DecPoint extends Input[Char] {
      override def data: Char = '.'
    }
  }

  sealed abstract case class Screen private (strNumber: String) {

    override def toString: String = if (strNumber.length > 1) strNumber.dropWhile(_ == '0') else strNumber

    def getNumber: BigDecimal = BigDecimal(strNumber, Screen.mc)

    def optConvertToNumWithPoint: BigDecimal =
      if (strNumber.last == '.')
        BigDecimal(s"$getNumber.0", Screen.mc)
      else getNumber
  }
  object Screen {
    val ScreenLength: Int = 10
    private val mc = new MathContext(ScreenLength, RoundingMode.UNNECESSARY)

    def of(strNumber: String): Option[Screen] =
      if (strNumber.count(_ != '.') <= ScreenLength)
        Try(BigDecimal(strNumber, mc)).toOption.map(_ => new Screen(strNumber) {})
      else None
  }

  sealed trait Operation
  object Operation {
    case object Plus     extends Operation
    case object Minus    extends Operation
    case object Multiply extends Operation
    case object Divide   extends Operation
  }
}
