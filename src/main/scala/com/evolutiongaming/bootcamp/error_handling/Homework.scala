package com.evolutiongaming.bootcamp.error_handling

import cats.data.ValidatedNec
import cats.syntax.all._

import scala.annotation.tailrec

// Homework. Place the solution under `error_handling` package in your homework repository.
//
// 1. Model `PaymentCard` class as an ADT (protect against invalid data as much as it makes sense).
// 2. Add `ValidationError` cases (at least 5, may be more).
// 3. Implement `validate` method to construct `PaymentCard` instance from the supplied raw data.
object Homework extends App {

  final case class User(name: String) extends AnyVal
  final case class CardNumber(number: String) extends AnyVal
  final case class CardExpDate(expirationDate: String) extends AnyVal
  object CardExpDate {
    def getNicelyFormatted(month: Int, year: Int): CardExpDate = CardExpDate(f"$month%02d/$year%02d")
  }
  final case class CardSecCode(securityCode: String) extends AnyVal

  final case class PaymentCard(owner: User, number: CardNumber, expirationDate: CardExpDate,
                               securityCode: CardSecCode)

  sealed trait ValidationError
  object ValidationError {
    final case object UserNameFormat extends ValidationError {
      override def toString: String = "Card owner name should contains name and surname, both starting from upper case"
    }
    final case object CardNumberFormat extends ValidationError {
      override def toString: String = "Card number should have exactly 16 digits"
    }
    final case object CardNumberIncorrect extends ValidationError {
      override def toString: String = "Card number is incorrect"
    }
    final case object CardExpDateFormat extends ValidationError {
      override def toString: String = "Card expiration date should be in format for example like these: 09/21, 09/3 or 9/03"
    }
    final case object CardExpDateBounds extends ValidationError {
      override def toString: String =
        """Card expiration date should contain month=m in range 1 <= m <= 12 and year=y in range 0 <= y <= 99
          |m/y
          |""".stripMargin
    }
    final case object CardSecCodeFormat extends ValidationError {
      override def toString: String = "Card secure code should have exactly 3 digits"
    }
  }

  object PaymentCardValidator {
    import ValidationError._

    type AllErrorsOr[A] = ValidatedNec[ValidationError, A]

    private def validateCardUser(name: String): AllErrorsOr[User] = {

      def validateCardUserName: AllErrorsOr[User] =
        if (name.matches("^[A-Z]{1}[a-z]* [A-Z]{1}[a-z]*$")) User(name).validNec
        else UserNameFormat.invalidNec

      validateCardUserName
    }

    private def validateCardNumber(number: String): AllErrorsOr[CardNumber] = {

      def validateCardNumberFormat: AllErrorsOr[CardNumber] =
        if (number.length == 16 && (number forall (_.isDigit))) CardNumber(number).validNec
        else CardNumberFormat.invalidNec

      def validateCardNumberCorrectness: AllErrorsOr[CardNumber] =
        if (Algorithms.isValidCardNumber(number)) CardNumber(number).validNec
        else CardNumberIncorrect.invalidNec

      validateCardNumberFormat.andThen(_ => validateCardNumberCorrectness)
    }

    private def validateCardExpDate(expirationDate: String): AllErrorsOr[CardExpDate] = {

      def validateCardExpDateFormat: AllErrorsOr[CardExpDate] = {
        val separated = expirationDate.split('/')
        if (separated.length == 2) {
          val (month, year) = (separated(0), separated(1))
          if (month.length <= 2 && (month forall (_.isDigit)) && year.length <= 2 && (year forall (_.isDigit)))
            CardExpDate.getNicelyFormatted(month.toInt, year.toInt).validNec
          else CardExpDateFormat.invalidNec
        }
        else CardExpDateFormat.invalidNec
      }

      def validateCardExpDateBounds: AllErrorsOr[CardExpDate] = {
        val separated = expirationDate.split('/')
        val (monthInt, yearInt) = (separated(0).toInt, separated(1).toInt)

        if (monthInt >= 1 && monthInt <= 12 && yearInt >= 0 && yearInt <= 99)
          CardExpDate.getNicelyFormatted(monthInt, yearInt).validNec
        else CardExpDateBounds.invalidNec
      }

      validateCardExpDateFormat.andThen(_ => validateCardExpDateBounds)
    }

    private def validateCardSecCode(securityCode: String): AllErrorsOr[CardSecCode] = {

      def validateCardSecCodeFormat: AllErrorsOr[CardSecCode] =
        if (securityCode.length == 3 && (securityCode forall (_.isDigit))) CardSecCode(securityCode).validNec
        else CardSecCodeFormat.invalidNec

      validateCardSecCodeFormat
    }

    def validate(
                  name: String,
                  number: String,
                  expirationDate: String,
                  securityCode: String,
                ): AllErrorsOr[PaymentCard] =
      (validateCardUser(name), validateCardNumber(number),
        validateCardExpDate(expirationDate), validateCardSecCode(securityCode)).mapN(PaymentCard)
  }

  object Algorithms {

    def isValidCardNumber(number: String): Boolean = {
      @tailrec
      def digitSum(number: String, oddSum: Int, evenSum: Int): Int = number.length match {
        case 0 => oddSum + evenSum
        case l if l % 2 == 0 =>
          val (exceptLastDigit, lastDigit) = number.splitAt(l - 1)
          digitSum(exceptLastDigit, oddSum, evenSum + lastDigit.toInt)
        case l =>
          val (exceptLastDigit, lastDigit) = number.splitAt(l - 1)
          val doubledDigit = lastDigit.toInt * 2
          digitSum(exceptLastDigit, oddSum + (doubledDigit / 10 + doubledDigit % 10), evenSum)
      }

      digitSum(number, 0, 0) % 10 == 0
    }
  }
  //import PaymentCardValidator.validate

  //println(validate("Alex Glowacki", "4847352989263094", "8/3", "000"))
}
