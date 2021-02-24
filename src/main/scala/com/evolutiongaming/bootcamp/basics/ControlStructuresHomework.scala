package com.evolutiongaming.bootcamp.basics

import scala.io.Source

object ControlStructuresHomework {
  // Homework

  // Create a command line application that reads various "commands" from the
  // stdin, evaluates them, and writes output to stdout.

  // Commands are:

  //   divide 4 5
  // which should output "4 divided by 5 is 0.8"

  //   sum 5 5 6 8.5
  // which should output "the sum of 5 5 6 8.5 is 24.5"

  //   average 4 3 8.5 4
  // which should output "the average of 4 3 8.5 4 is 4.875"

  //   min 4 -3 -17
  // which should output "the minimum of 4 -3 -17 is -17"

  //   max 4 -3 -17
  // which should output "the maximum of 4 -3 -17 is 4"

  // In case of commands that cannot be parsed or calculations that cannot be performed,
  // output a single line starting with "Error: "

  sealed trait Command
  object Command {
    final case class Divide(dividend: Double, divisor: Double) extends Command
    final case class Sum(numbers: List[Double]) extends Command
    final case class Average(numbers: List[Double]) extends Command
    final case class Min(numbers: List[Double]) extends Command
    final case class Max(numbers: List[Double]) extends Command
  }

  final case class ErrorMessage(value: String)

  // Adjust `Result` and `ChangeMe` as you wish - you can turn Result into a `case class` and remove the `ChangeMe` if
  // you think it is the best model for your solution, or just have other `case class`-es implement `Result`
  //final case class Result(value: Double)
  sealed trait Result
  object Result {
    final case class Division(res: Double, dividend: Double, divisor: Double) extends Result
    final case class Sum(res: Double, numbers: List[Double]) extends Result
    final case class Average(res: Double, numbers: List[Double]) extends Result
    final case class Min(res: Double, numbers: List[Double]) extends Result
    final case class Max(res: Double, numbers: List[Double]) extends Result
  }

  final case class ChangeMe(value: String) extends Result {
    def toDouble: Either[ErrorMessage, Double] = value.toDoubleOption match {
      case Some(num) => Right(num)
      case None => Left(ErrorMessage("Error: String argument can't be converted to Double"))
    }
    def toDoubles: Either[ErrorMessage, List[Double]] = {
      val args = value.split("\\s+").toList
      // In place of not parsable strings are empty sequences
      val seqs = args map (ChangeMe(_).toDouble.toSeq)
      if (seqs.exists(_.isEmpty)) Left(ErrorMessage("Error: String argument can't be converted to Double"))
      else Right(seqs.flatten)
    }
  }

  def parseCommand(x: String): Either[ErrorMessage, Command] = x.toLowerCase.split("\\s+").toList match {
    case "divide" :: arg1 :: arg2 :: Nil => for {
      num1 <- ChangeMe(arg1).toDouble
      num2 <- ChangeMe(arg2).toDouble
    } yield Command.Divide(num1, num2)

    case "sum" :: arg1 :: rest => for {
      nums <- ChangeMe((arg1 :: rest).mkString(" ")).toDoubles
    } yield Command.Sum(nums)

    case "average" :: arg1 :: rest => for {
      nums <- ChangeMe((arg1 :: rest).mkString(" ")).toDoubles
    } yield Command.Average(nums)

    case "min" :: arg1 :: rest => for {
      nums <- ChangeMe((arg1 :: rest).mkString(" ")).toDoubles
    } yield Command.Min(nums)

    case "max" :: arg1 :: rest => for {
      nums <- ChangeMe((arg1 :: rest).mkString(" ")).toDoubles
    } yield Command.Max(nums)

    case _ => Left(ErrorMessage("Error: The command is not recognizable"))
  }

  // should return an error (using `Left` channel) in case of division by zero and other
  // invalid operations
  def calculate(x: Command): Either[ErrorMessage, Result] = x match {
    case Command.Divide(dividend, divisor) =>
      if (divisor == 0) Left(ErrorMessage("Division by 0 is prohibited"))
      else Right(Result.Division(dividend / divisor, dividend, divisor))

    case Command.Sum(nums) => Right(Result.Sum(nums.sum, nums))
    case Command.Average(nums) => Right(Result.Average(nums.sum / nums.length, nums))
    case Command.Min(nums) => Right(Result.Min(nums.min, nums))
    case Command.Max(nums) => Right(Result.Max(nums.max, nums))
  }

  // Unchecked for `ChangeMe` case class
  def renderResult(x: Result): String = (x: @unchecked) match {
    case Result.Division(res, dividend, divisor) => s"$dividend divided by $divisor is $res"
    case Result.Sum(res, nums) => s"""the sum of ${nums.mkString(" ")} is $res"""
    case Result.Average(res, nums) => s"""the average of ${nums.mkString(" ")} is $res"""
    case Result.Min(res, nums) => s"""the minimum of ${nums.mkString(" ")} is $res"""
    case Result.Max(res, nums) => s"""the maximum of ${nums.mkString(" ")} is $res"""
  }

  def process(x: String): String = {
    import cats.implicits._
    // the import above will enable useful operations on Either-s such as `leftMap`
    // (map over the Left channel) and `merge` (convert `Either[A, A]` into `A`),
    // but you can also avoid using them using pattern matching.
    (for {
      cmd <- parseCommand(x: String)
      res <- calculate(cmd)
    } yield renderResult(res)).leftMap(_.value).merge
  }

  // This `main` method reads lines from stdin, passes each to `process` and outputs the return value to stdout
  def main(args: Array[String]): Unit = Source.stdin.getLines() map process foreach println
}
