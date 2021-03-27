package com.evolutiongaming.bootcamp.testing2

import com.evolutiongaming.bootcamp.testing2.Calculator.Input
import com.evolutiongaming.bootcamp.testing2.Calculator.Input.Digit._
import com.evolutiongaming.bootcamp.testing2.Calculator.Input._
import com.evolutiongaming.bootcamp.testing2.Calculator.Screen.ScreenLength
import org.scalacheck.Gen
import org.scalatest.{EitherValues, OptionValues}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import java.math.{MathContext, RoundingMode}
import scala.util.Random

trait CalculatorUtils {
  import Calculator.Screen

  val starterCalc = Calculator()
  val calcWithPoint = Calculator(screen = Screen.of("0.").get)
  val mc = new MathContext(ScreenLength, RoundingMode.UNNECESSARY)

  def enterNumber[A](calc: Calculator, number: List[Input[A]]): Calculator =
    number.foldLeft(calc) {
      case (c: Calculator, d: Digit) => c.enterDigit(d)
      case (c, _)                    => c.enterDecPoint
    }
}

// @nowarn
class CalculatorSpec
  extends AnyWordSpec
    with Matchers
    with OptionValues
    with EitherValues
    with ScalaCheckDrivenPropertyChecks
    with CalculatorUtils {

  // Generators
  val digitGen = Gen.oneOf(Zero, One, Two, Three, Four, Five, Six, Seven, Eight, Nine)

  val numbersWithoutPointGen = for {
    len    <- Gen.choose(1, 5)
    number <- Gen.listOfN(len, digitGen)
    if number.head != Digit.Zero
  } yield number

  val numbersWithPointGen = for {
    len         <- Gen.choose(2, 6)
    lenBefore   = Random.between(1, len)
    beforePoint <- Gen.listOfN(lenBefore, digitGen)
    if beforePoint.head != Digit.Zero
    lenAfter    = len - lenBefore
    afterPoint  <- Gen.listOfN(lenAfter, digitGen)
  } yield beforePoint ::: DecPoint :: afterPoint

  val numbersGen = Gen.oneOf(numbersWithoutPointGen, numbersWithPointGen)

  // Tests
  "Calculator" should {
    "enter the numbers correctly" in {
      forAll(numbersGen) { number =>
        val screen = enterNumber(starterCalc, number).screen

        screen.toString shouldEqual number.mkString
      }
    }

    "does nothing" when {
      "you just repeat pressing `=`" in {
        for {
          calc1 <- starterCalc.calculate
          calc2 <- calc1.calculate
          calc3 <- calc2.calculate
          calc4 <- calc3.calculate
        } yield calc4 shouldEqual starterCalc
      }
    }

    "calculate sum correctly" when {
      "operands are integers" in {
        forAll(numbersWithoutPointGen, numbersWithoutPointGen) { (number1, number2) =>
          val calcAfterFirstNumber  = enterNumber(starterCalc, number1)
          val calcAfterPlus         = calcAfterFirstNumber.plus
          val calcAfterSecondNumber = enterNumber(calcAfterPlus, number2)

          val (num1, num2) = (BigDecimal(number1.mkString, mc), BigDecimal(number2.mkString, mc))
          calcAfterSecondNumber.calculate.right.value.screen.getNumber shouldEqual (num1 + num2)
        }
      }
      "operands are floats" in {
        forAll(numbersWithPointGen, numbersWithPointGen) { (number1, number2) =>
          val calcAfterFirstNumber  = enterNumber(starterCalc, number1)
          val calcAfterPlus         = calcAfterFirstNumber.plus
          val calcAfterSecondNumber = enterNumber(calcAfterPlus, number2)

          val (num1, num2) = (BigDecimal(number1.mkString, mc), BigDecimal(number2.mkString, mc))
          calcAfterSecondNumber.calculate.right.value.screen.getNumber shouldEqual (num1 + num2)
        }
      }
    }

    "calculate difference correctly" when {
      "operands are integers" in {
        forAll(numbersWithoutPointGen, numbersWithoutPointGen) { (number1, number2) =>
          val calcAfterFirstNumber  = enterNumber(starterCalc, number1)
          val calcAfterMinus        = calcAfterFirstNumber.minus
          val calcAfterSecondNumber = enterNumber(calcAfterMinus, number2)

          val (num1, num2) = (BigDecimal(number1.mkString, mc), BigDecimal(number2.mkString, mc))
          calcAfterSecondNumber.calculate.right.value.screen.getNumber shouldEqual (num1 - num2)
        }
      }
      "operands are floats" in {
        forAll(numbersWithPointGen, numbersWithPointGen) { (number1, number2) =>
          val calcAfterFirstNumber  = enterNumber(starterCalc, number1)
          val calcAfterMinus        = calcAfterFirstNumber.minus
          val calcAfterSecondNumber = enterNumber(calcAfterMinus, number2)

          val (num1, num2) = (BigDecimal(number1.mkString, mc), BigDecimal(number2.mkString, mc))
          calcAfterSecondNumber.calculate.right.value.screen.getNumber shouldEqual (num1 - num2)
        }
      }
    }

    "calculate product correctly" when {
      "operands are integers" in {
        forAll(numbersWithoutPointGen, numbersWithoutPointGen) { (number1, number2) =>
          val calcAfterFirstNumber     = enterNumber(starterCalc, number1)
          val calcAfterMultiply        = calcAfterFirstNumber.multiply
          val calcAfterSecondNumber    = enterNumber(calcAfterMultiply, number2)

          val (num1, num2) = (BigDecimal(number1.mkString, mc), BigDecimal(number2.mkString, mc))
          calcAfterSecondNumber.calculate.right.value.screen.getNumber shouldEqual (num1 * num2)
        }
      }
      "operands are floats" in {
        forAll(numbersWithPointGen, numbersWithPointGen) { (number1, number2) =>
          val calcAfterFirstNumber     = enterNumber(starterCalc, number1)
          val calcAfterMultiply        = calcAfterFirstNumber.multiply
          val calcAfterSecondNumber    = enterNumber(calcAfterMultiply, number2)

          val (num1, num2) = (BigDecimal(number1.mkString, mc), BigDecimal(number2.mkString, mc))
          calcAfterSecondNumber.calculate.right.value.screen.getNumber shouldEqual (num1 * num2)
        }
      }
    }

    "print `ERR - division by 0` string when trying to divide by 0" in {
      forAll(numbersGen) { number =>
        val calcAfterNumber = enterNumber(starterCalc, number)
        val calcAfterOp     = calcAfterNumber.divide
        val calcAfterZero   = calcAfterOp.enterDigit(Zero)

        calcAfterZero.calculate.left.value shouldEqual "ERR - division by 0"
      }
    }

    "displays 0" when {
      "you are pressing same 0s at the beginning" in {
        val calc1 = starterCalc.enterDigit(Zero)
        val calc2 = calc1.enterDigit(Zero)
        val calc3 = calc2.enterDigit(Zero)
        val calc4 = calc3.enterDigit(Zero)

        calc4.screen.toString shouldEqual starterCalc.screen.toString
      }
    }

    "calculate correctly chain of calculations" in {
      // 2.356 + 1000.9 - 6 * 423.23 / 10 = 42206.865
      val number1 = List(Two, DecPoint, Three, Five, Six)
      val number2 = List(One, Zero, Zero, Zero, DecPoint, Nine)
      val number3 = List(Six)
      val number4 = List(Four, Two, Three, DecPoint, Two, Three)
      val number5 = List(One, Zero)

      val calc1 = enterNumber(starterCalc, number1)
      val calc2 = calc1.plus
      val calc3 = enterNumber(calc2, number2)
      for {
        calc4 <- calc3.calculate
        calc5   = calc4.minus
        calc6   = enterNumber(calc5, number3)
        calc7  <- calc6.calculate
        calc8   = calc7.multiply
        calc9   = enterNumber(calc8, number4)
        calc10 <- calc9.calculate
        calc11  = calc10.divide
        calc12  = enterNumber(calc11, number5)
        calc13 <- calc12.calculate
      } yield calc13.screen.getNumber shouldEqual BigDecimal(42206.865, mc)
    }
  }
}
