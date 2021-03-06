package com.evolutiongaming.bootcamp.json

import java.time.format.DateTimeFormatter
import java.time.{LocalDate, ZonedDateTime}
import cats.instances.either._
import cats.instances.list._
import cats.syntax.traverse._
import io.circe
import io.circe.{Decoder, Encoder}
import io.circe.parser._
import io.circe.generic.JsonCodec
import org.scalatest.EitherValues
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import scalaj.http.Http

import scala.util.Try
import io.circe.generic.extras._
/**
 * HOMEWORK:
 *
 * Some classes and generated JSON codecs are provided for NBA API.
 * Unfortunately, they don't work as expected out of the box.
 * The task is to fix (rewrite) some of the codecs to make tests pass.
 * You are not supposed to change anything in _class_ HomeworkSpec,
 * instead of it you are supposed to keep your changes inside _companion object_ for HomeworkSpec.
 *
 * You are not allowed to rename fields in case classes.
 * You are not allowed to remove fields from case classes.
 * You are supposed to use camelCase if you introduce additional fields in case classes.
 *
 * It would be nice to avoid using Encoder/Decoder.forProductN where you specify all field names
 */
class HomeworkSpec extends AnyWordSpec with Matchers with EitherValues {
  import HomeworkSpec._

  "NBA JSON API client" should {
    "get info about today games" in {
      val date = LocalDate.now()
      val scoreboardOrError = fetchScoreboard(date)
      val scoreboard = scoreboardOrError.getOrElse(fail(scoreboardOrError.toString))
      val allGameIds = scoreboard.games.map(_.gameId)
      val gameInfosOrError = allGameIds.map(fetchGameInfo(date, _)).sequence
      gameInfosOrError.getOrElse(fail(gameInfosOrError.toString))
      succeed
    }

    "fetch games for 14 Feb 2020" in {
      val date = LocalDate.of(2020, 2, 14)
      val scoreboardOrError = fetchScoreboard(date)
      val scoreboard = scoreboardOrError.getOrElse(fail(scoreboardOrError.toString))
      val allGameIds = scoreboard.games.map(_.gameId)
      val gameInfosOrError = allGameIds.map(fetchGameInfo(date, _)).sequence
      val gameInfos = gameInfosOrError.getOrElse(fail(gameInfosOrError.toString))
      gameInfos.size must be(1)
    }
  }

}

object HomeworkSpec {
  implicit val config: Configuration = Configuration.default
  @ConfiguredJsonCodec final case class TeamTotals(
                                                    assists: String,
                                                    @JsonKey("full_timeout_remaining") fullTimeoutRemaining: String,
                                                    plusMinus: String
                                                  )

  @JsonCodec final case class TeamBoxScore(totals: TeamTotals)
  @JsonCodec final case class GameStats(hTeam: TeamBoxScore, vTeam: TeamBoxScore)

  implicit val localDateEncoder: Encoder[LocalDate] = Encoder.encodeString.contramap(_.toString)
  implicit val localDateDecoder: Decoder[LocalDate] = Decoder.decodeString.emap { str =>
    lazy val formatter = DateTimeFormatter.ofPattern("yyyyMMdd")
    Try(LocalDate.parse(str, formatter)).toEither.left.map(_.getMessage)
  }
  @JsonCodec final case class PrevMatchup(gameDate: LocalDate, gameId: String)
  @JsonCodec final case class BoxScore(
                                        basicGameData: Game,
                                        previousMatchup: PrevMatchup,
                                        stats: Option[GameStats],
                                      )
  @JsonCodec final case class JustScore(score: String)  // checked
  @JsonCodec final case class TeamStats(
                                         linescore: List[JustScore],        // checked
                                         loss: String,                      // checked
                                         score: String,                     // checked
                                         teamId: String,                    // checked
                                         triCode: String                    // checked
                                       )
  @JsonCodec final case class GameDuration(hours: String, minutes: String) // checked
  @JsonCodec final case class Arena(
                                     city: String,                      // checked
                                     country: String,                   // checked
                                     isDomestic: Boolean,               // checked
                                     name: String,                      // checked
                                     stateAbbr: String                  // checked
                                   )
  @JsonCodec final case class Game(
                                    arena: Arena,                      // checked
                                    attendance: String,                // checked
                                    endTimeUTC: Option[ZonedDateTime], // checked
                                    gameDuration: GameDuration,        // checked
                                    gameId: String,                    // checked
                                    gameUrlCode: String,               // checked
                                    hTeam: TeamStats,                  // checked
                                    isBuzzerBeater: Boolean,           // checked
                                    startTimeUTC: ZonedDateTime,       // checked
                                    vTeam: TeamStats,                  // checked
                                  )
  @JsonCodec final case class Scoreboard(games: List[Game], numGames: Int) // checked

  private def fetchScoreboard(date: LocalDate): Either[circe.Error, Scoreboard] = {
    val dateString = date.format(DateTimeFormatter.BASIC_ISO_DATE)
    val body = Http(s"https://data.nba.net/10s/prod/v1/$dateString/scoreboard.json").asString.body
    decode[Scoreboard](body)
  }

  private def fetchGameInfo(date: LocalDate, gameId: String): Either[circe.Error, BoxScore] = {
    val dateString = date.format(DateTimeFormatter.BASIC_ISO_DATE)
    val body = Http(s"https://data.nba.net/10s/prod/v1/$dateString/${gameId}_boxscore.json").asString.body
    decode[BoxScore](body)
  }
}
