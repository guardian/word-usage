package com.gu.words

import cats.data.EitherT
import cats.effect.*
import cats.syntax.all.*
import com.gu.time.duration.formatting.*
import com.gu.words.capi.ContentService
import com.gu.words.model.YearMonth
import com.gu.words.reports.{DayReport, MonthReport, WordUsage, given}
import com.gu.words.store.Store
import kantan.csv.*

import java.nio.file.{Files, Path}
import java.time.LocalDate
import java.time.temporal.ChronoUnit.MILLIS
import scala.collection.immutable.SortedMap
import scala.concurrent.duration.DurationInt
import scala.jdk.DurationConverters.*
import scala.language.postfixOps

extension (path: Path)
  def addExtension(suffix: String): Path = path.resolveSibling(s"${path.getFileName}.$suffix")

extension [T] (io: IO[T])
  def logTime(desc: String): IO[T] = IO.println(s"$desc...") >> io.timed.flatMap {
    case (d, v) => IO.println(s"$desc ...finished in ${d.toJava.truncatedTo(MILLIS).format()}") >> IO.pure(v)
  }

  def logIfSlow(desc: String, threshold: scala.concurrent.duration.FiniteDuration = 1.millis): IO[T] = io.timed.flatMap {
    case (d, v) => IO.whenA(d > threshold)(IO.println(s"$desc ...finished in ${d.toJava.truncatedTo(MILLIS).format()}")) >> IO.pure(v)
  }

class ExtractorService(folder: Path, contentService: ContentService, store: Store) {

  def pathFor(yearMonth: YearMonth): Path = folder.resolve(f"day-month-data/${yearMonth.year}/${yearMonth.month.getValue}%02d.csv.gz")

  def pathForWordUsageSummary(): Path = folder.resolve("summary.csv")
  def pathForDetailedWordReport(word: Word): Path = folder.resolve(s"words/${word.take(3).mkString("/")}/$word.csv.gz")

  
  def process(start: YearMonth, end: YearMonth): IO[Unit] = fs2.Stream(start.to(end)*).covary[IO]
    .parEvalMap(2)(yearMonth => fetchFromCapiOrLoadFromCache(yearMonth).value.map(yearMonth -> _))
    .evalFold(Map.empty[Word, WordUsage]) {
      case (previouslySeen, (yearMonth, dayDataEitherFetchedOrLoadedFromFileCache)) =>
        integrateMonthDataAndUpdateSeenWordsReport(previouslySeen, yearMonth, dayDataEitherFetchedOrLoadedFromFileCache)
    }.compile.lastOrError.flatTap {
      seen =>
        val path = pathForWordUsageSummary()

        (IO.println(s"Writing to ${path.toAbsolutePath}") >>
            IO.blocking(WordUsage.writeSummary(path, seen))).logTime("Write summary") >>
          seen.toSeq.filter(_._2.count >= 10000).traverse {
            case (word, wordUsage) =>
              store.write(pathForDetailedWordReport(word), wordUsage.detailedUsageCsvRows)
          }.logTime("Write detailed word reports")
    }.void

  private def integrateMonthDataAndUpdateSeenWordsReport(
    previouslySeen: Map[Word, WordUsage],
    yearMonth: YearMonth,
    fetchedDayReportsOrCachedMonthReport: Either[Map[LocalDate, DayReport], MonthReport]
  ): IO[Map[Word, WordUsage]] = for {
    monthReport <- IO(fetchedDayReportsOrCachedMonthReport.leftMap(dayReports =>
      MonthReport.from(yearMonth, SortedMap.from(dayReports), previouslySeen)).merge)
    _ <- IO.whenA(fetchedDayReportsOrCachedMonthReport.isLeft)(writeMonthReport(yearMonth, monthReport)).start
  } yield updatedWordReportFor(previouslySeen, yearMonth, monthReport)

  private def updatedWordReportFor(previouslySeen: Map[Word, WordUsage], yearMonth: YearMonth, monthReport: MonthReport): Map[Word, WordUsage] = previouslySeen ++ (for {
    (word, occ) <- monthReport.occurrenceByWord
  } yield word -> previouslySeen.get(word).fold(WordUsage.fromFirstOccurrenceInRun(yearMonth, occ))(_.add(yearMonth, occ)))

  private def writeMonthReport(yearMonth: YearMonth, monthReport: MonthReport): IO[Unit] =
    store.write(pathFor(yearMonth), monthReport.csvRows).logTime(s"$yearMonth : Storing to disk")

  def fetchFromCapiOrLoadFromCache(yearMonth: YearMonth): EitherT[IO, Map[LocalDate, DayReport], MonthReport] =
    EitherT.right(IO.blocking(Files.isRegularFile(pathFor(yearMonth)))).flatMap { fileExists =>
      val fetchFresh = contentService.fetchDayReportsFor(yearMonth).logTime(s"$yearMonth : Fetching from CAPI")
      if (fileExists) EitherT(loadCachedFile(yearMonth).attempt).leftSemiflatMap(_ => fetchFresh)
      else EitherT.left(fetchFresh)
    }

  private def loadCachedFile(yearMonth: YearMonth): IO[MonthReport] =
    store.read(pathFor(yearMonth)).map(s => MonthReport(s.toMap)).logTime(s"$yearMonth : Loading from disk")
}
