package com.evolutiongaming.bootcamp.basics

import com.evolutiongaming.bootcamp.basics.ControlStructuresHomework._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class ControlStructuresHomeworkSpec extends AnyFlatSpec with ScalaCheckDrivenPropertyChecks {

  "parseCommand" should "print error message for unknown commands" in {
    val cmd1 = "modulo 2 3"
    val cmd2 = " sum 2"
    val cmd3 = "divide 2"
    val cmd4 = "sum"
    val cmd5 = "average  "

    parseCommand(cmd1) shouldEqual Left(ErrorMessage("Error: The command is not recognizable"))
    parseCommand(cmd2) shouldEqual Left(ErrorMessage("Error: The command is not recognizable"))
    parseCommand(cmd3) shouldEqual Left(ErrorMessage("Error: The command is not recognizable"))
    parseCommand(cmd4) shouldEqual Left(ErrorMessage("Error: The command is not recognizable"))
    parseCommand(cmd5) shouldEqual Left(ErrorMessage("Error: The command is not recognizable"))
  }

  "parseCommand" should "print error message for not parsable arguments" in {
    val cmd1 = "sum two three"
    val cmd2 = "average 1.000 2 three"

    parseCommand(cmd1) shouldEqual Left(ErrorMessage("Error: String argument can't be converted to Double"))
    parseCommand(cmd2) shouldEqual Left(ErrorMessage("Error: String argument can't be converted to Double"))
  }

  "parseCommand" should "accept parsable arguments" in {
    val cmd1 = "sum 1 2 3 4"
    val cmd2 = "divide 2.500 2.13"

    parseCommand(cmd1) shouldEqual Right(Command.Sum(List(1, 2, 3, 4)))
    parseCommand(cmd2) shouldEqual Right(Command.Divide(2.5, 2.13))
  }

  "process" should "work correctly for example inputs" in {
    val in1 = "divide 4 5"
    val in2 = "sum 5 5 6 8.5"
    val in3 = "average 4 3 8.5 4"
    val in4 = "min 4 -3 -17"
    val in5 = "max 4 -3 -17"
    val in6 = "average 5"

    process(in1) shouldEqual "4.0 divided by 5.0 is 0.8"
    process(in2) shouldEqual "the sum of 5.0 5.0 6.0 8.5 is 24.5"
    process(in3) shouldEqual "the average of 4.0 3.0 8.5 4.0 is 4.875"
    process(in4) shouldEqual "the minimum of 4.0 -3.0 -17.0 is -17.0"
    process(in5) shouldEqual "the maximum of 4.0 -3.0 -17.0 is 4.0"
    process(in6) shouldEqual "the average of 5.0 is 5.0"
  }
}
