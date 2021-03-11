package com.evolutiongaming.bootcamp.error_handling

import cats.syntax.all._
import com.evolutiongaming.bootcamp.error_handling.Homework._
import org.scalatest.Assertion
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class HomeworkSpec extends AnyFlatSpec with Matchers {

  "PaymentCardValidator" should "handle valid and invalid cards" in {
    import ValidationError._

    PaymentCardValidator.validate(
      name = "Alex Glowacki",
      number = "4847352989263094",
      expirationDate = "8/3",
      securityCode = "000"
    ) shouldBe PaymentCard(User("Alex Glowacki"), CardNumber("4847352989263094"), CardExpDate("08/03"), CardSecCode("000")).validNec

    def checkInvalid(name: String, number: String, expirationDate: String, securityCode: String,
                     errors: Set[ValidationError]): Assertion =
      PaymentCardValidator.validate(
        name = name,
        number = number,
        expirationDate = expirationDate,
        securityCode = securityCode
      ).leftMap(_.toList.toSet) shouldBe errors.invalid

    checkInvalid(
      name = "Uga",
      number = "4847352989263094",
      expirationDate = "8/3",
      securityCode = "000",
      errors = Set(UserNameFormat)
    )

    checkInvalid(
      name = "Cezary",
      number = "4l473529o92630l4",
      expirationDate = "8/3",
      securityCode = "000",
      errors = Set(UserNameFormat, CardNumberFormat)
    )

    checkInvalid(
      name = "Cezary",
      number = "4847352989263095",
      expirationDate = "8/3",
      securityCode = "000",
      errors = Set(UserNameFormat, CardNumberIncorrect)
    )

    checkInvalid(
      name = "Cezary",
      number = "4847352989263095",
      expirationDate = "00008/3",
      securityCode = "000",
      errors = Set(UserNameFormat, CardNumberIncorrect, CardExpDateFormat)
    )

    checkInvalid(
      name = "Cezary",
      number = "4847352989263095",
      expirationDate = "21/3",
      securityCode = "000",
      errors = Set(UserNameFormat, CardNumberIncorrect, CardExpDateBounds)
    )

    checkInvalid(
      name = "Cezary",
      number = "4847352989263095",
      expirationDate = "21/3",
      securityCode = "0",
      errors = Set(UserNameFormat, CardNumberIncorrect, CardExpDateBounds, CardSecCodeFormat)
    )
  }
}
