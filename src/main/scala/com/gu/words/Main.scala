package com.gu.words

import cats.data.*
import cats.effect.*
import cats.effect.std.Env
import cats.syntax.all.*
import com.gu.contentapi.client.catseffect.IOCapiClient
import com.gu.words.capi.ContentService
import com.gu.words.model.{YearMonth, yearMonth}
import com.gu.words.store.FileStore
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.slf4j.Slf4jFactory

import java.nio.file.Path
import java.time.LocalDate
import scala.concurrent.ExecutionContext.Implicits.global

type Word = String

object Main extends IOApp {
  val CAPI_KEY_ENV_VAR = "WORD_USAGE_CAPI_KEY"
  type ValidationResult[A] = ValidatedNec[String, A]
  implicit val logging: LoggerFactory[IO] = Slf4jFactory.create[IO]

  def run(args: List[String]): IO[ExitCode] = (for {
    apiKey <- EitherT.fromOptionF(Env[IO].get(CAPI_KEY_ENV_VAR), s"Please set the `$CAPI_KEY_ENV_VAR` environment variable!")
    dates <- EitherT.fromEither[IO](parseDateRange(args).toEither.leftMap(_.mkString_("\n")))
    _ <- EitherT.right {
      for {
        capiClient <- IOCapiClient.from(apiKey)
        service = new ExtractorService(Path.of("output"), new ContentService(capiClient), new FileStore)
        (startDate, endDate) = dates
        _ <- service.process(startDate, endDate).logTime(s"Processing $startDate to $endDate")
      } yield ()
    }
  } yield ExitCode.Success).valueOrF(err => IO.consoleForIO.error(err) >> IO.pure(ExitCode.Error))

  def parseDateRange(args: List[String]): ValidationResult[(YearMonth, YearMonth)] = args.traverse(parseYearMonth).map {
    case start :: end :: Nil => (start, end)
    case single :: Nil => (single, single)
    case _ =>
      val now = LocalDate.now().yearMonth
      (now, now)
  }

  def parseYearMonth(text: String): ValidationResult[YearMonth] =
    Validated.fromOption(YearMonth.parse(text), NonEmptyChain(s"Could not parse year-month (in 'YYYY-MM' format) from '$text'"))
}
