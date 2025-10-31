package com.gu.words

import cats.data.*
import cats.effect.*
import cats.syntax.all.*
import com.gu.time.duration.formatting.*
import com.gu.words.capi.ContentService
import com.gu.words.model.YearMonth
import com.gu.words.reports.{DayReport, MonthReport, WordReport}
import kantan.csv.CsvSink

import java.io.BufferedInputStream
import java.nio.file.Files.newInputStream
import java.nio.file.StandardCopyOption.REPLACE_EXISTING
import java.nio.file.{Files, Path}
import java.time.LocalDate
import java.time.temporal.ChronoUnit.MILLIS
import java.util.zip.{GZIPInputStream, GZIPOutputStream}
import scala.collection.immutable.SortedMap
import scala.jdk.DurationConverters.*
import scala.language.postfixOps
import scala.util.{Random, Try, Using}

extension (path: Path)
  def addExtension(suffix: String): Path = path.resolveSibling(s"${path.getFileName}.$suffix")

extension [T] (io: IO[T])
  def logTime(desc: String): IO[T] = IO.println(s"$desc...") >> io.timed.flatMap {
    case (d, v) => IO.println(s"$desc ...finished in ${d.toJava.truncatedTo(MILLIS).format()}") >> IO.pure(v)
  }

  def logSlow(desc: String, threshold: scala.concurrent.duration.FiniteDuration): IO[T] = io.timed.flatMap {
    case (d, v) => IO.whenA(d > threshold)(IO.println(s"$desc ...finished in ${d.toJava.truncatedTo(MILLIS).format()}")) >> IO.pure(v)
  }

class ExtractorService(folder: Path, contentService: ContentService) {

  def pathFor(yearMonth: YearMonth): Path = folder.resolve(f"day-month-data/${yearMonth.year}/${yearMonth.month.getValue}%02d.csv.gz")

  def pathForWordUsageSummary(): Path = folder.resolve(f"summary.csv")

  
  def process(start: YearMonth, end: YearMonth): IO[Unit] = fs2.Stream(start.to(end)*).covary[IO]
    .parEvalMap(2)(yearMonth => fetchFromCapiOrLoadFromCache(yearMonth).value.map(yearMonth -> _))
    .evalFold(Map.empty[Word, WordReport]) {
      case (previouslySeen, (yearMonth, dayDataEitherFetchedOrLoadedFromFileCache)) =>
        integrateMonthDataAndUpdateSeenWordsReport(previouslySeen, yearMonth, dayDataEitherFetchedOrLoadedFromFileCache)
    }.compile.lastOrError.flatTap {
      seen =>
        val path = pathForWordUsageSummary()

        (IO.println(s"Writing to ${path.toAbsolutePath}") >>
            IO.blocking(WordReport.write(path, seen))).logTime("Write summary")
    }.void

  
  private def integrateMonthDataAndUpdateSeenWordsReport(
    previouslySeen: Map[Word, WordReport],
    yearMonth: YearMonth,
    fetchedDayReportsOrCachedMonthReport: Either[Map[LocalDate, DayReport], MonthReport]
  ): IO[Map[Word, WordReport]] = for {
    monthReport <- IO(fetchedDayReportsOrCachedMonthReport.leftMap(dayReports =>
      MonthReport.from(yearMonth, SortedMap.from(dayReports), previouslySeen)).merge)
    _ <- IO.whenA(fetchedDayReportsOrCachedMonthReport.isLeft)(writeMonthReport(yearMonth, monthReport)).start
  } yield updatedWordReportFor(previouslySeen, yearMonth, monthReport)

  private def updatedWordReportFor(previouslySeen: Map[Word, WordReport], yearMonth: YearMonth, monthReport: MonthReport): Map[Word, WordReport] = previouslySeen ++ (for {
    (word, occ) <- monthReport.occurrenceByWord
  } yield word -> previouslySeen.get(word).fold(WordReport.fromFirstOccurrenceInRun(yearMonth, occ))(_.add(occ)))

  private def writeMonthReport(yearMonth: YearMonth, monthReport: MonthReport): IO[Unit] = IO.blocking {
    val path = pathFor(yearMonth)
    Files.createDirectories(path.getParent)

    val tmpPath = path.addExtension(s"${Random.alphanumeric.take(5).mkString}.tmp")
    Using(new GZIPOutputStream(Files.newOutputStream(tmpPath)))(monthReport.write)
    Files.move(tmpPath, path, REPLACE_EXISTING)
  }.void.logTime(s"$yearMonth : Storing to disk")

  def fetchFromCapiOrLoadFromCache(yearMonth: YearMonth): EitherT[IO, Map[LocalDate, DayReport], MonthReport] =
    EitherT.right(IO.blocking(Files.isRegularFile(pathFor(yearMonth)))).flatMap { fileExists =>
      val fetchFresh = contentService.fetchDayReportsFor(yearMonth).logTime(s"$yearMonth : Fetching from CAPI")

      if (fileExists) EitherT(loadCachedFile(yearMonth).map(_.toEither)).leftSemiflatMap(_ => fetchFresh)
      else EitherT.left(fetchFresh)
    }

  private def loadCachedFile(yearMonth: YearMonth): IO[Try[MonthReport]] = {
    val punk = 8 * 1024
    val path = pathFor(yearMonth)
    IO.blocking {
      Using(new BufferedInputStream(new GZIPInputStream(newInputStream(path), punk), 4 * punk))(MonthReport.read)
    }.logTime(s"$yearMonth : Loading from disk")
  }
}
