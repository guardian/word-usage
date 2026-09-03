package com.gu.words.reports

import cats.effect.IO
import com.gu.contentapi.client.model.CapiId
import com.gu.words.Word
import com.gu.words.model.YearMonth
import com.gu.words.reports.MonthReport.Occurrence.FreqBucket
import com.gu.words.reports.WordUsage.FirstUse
import kantan.csv.*
import kantan.csv.java8.*
import kantan.csv.ops.*

import java.time.LocalDate
import scala.collection.immutable.SortedMap

case class WordUsage(firstUse: FirstUse, monthOccurrences: Map[YearMonth, MonthReport.Occurrence]) {
  def add(yearMonth: YearMonth, monthOccurrence: MonthReport.Occurrence): WordUsage =
    copy(monthOccurrences = monthOccurrences + (yearMonth -> monthOccurrence))

  lazy val count: Long = monthOccurrences.map(_._2.count.toLong).sum

  def detailedUsageCsvRows: Iterable[(YearMonth, MonthReport.Occurrence)] =
    monthOccurrences.toSeq.sortBy(_._1)
}

case object WordUsage {
  case class FirstUse(day: LocalDate, capiId: CapiId)

  object FirstUse {
    def from(yearMonth: YearMonth, monthReport: MonthReport.Occurrence): FirstUse = FirstUse(
      yearMonth.onDayOfMonth(monthReport.dayOfFirstUseInMonth),
      monthReport.firstSeenDuringRun.get // I'm so naughty!
    )
  }

  def fromFirstOccurrenceInRun(yearMonth: YearMonth, monthReport: MonthReport.Occurrence) =
    WordUsage(FirstUse.from(yearMonth, monthReport), Map(yearMonth -> monthReport))

  def writeSummary[A: CsvSink](sink: A, reportsByWord: Map[Word, WordUsage]): IO[Unit] = IO.blocking {
    given RowEncoder[(Word, WordUsage)] =
      RowEncoder.encoder[(Word, WordUsage), Word, Long, LocalDate, CapiId](0, 1, 2, 3) {
        r => (r._1, r._2.count, r._2.firstUse.day, r._2.firstUse.capiId)
      }

    sink.writeCsv(SortedMap.from(reportsByWord), rfc)
  }
}

given RowEncoder[(YearMonth, MonthReport.Occurrence)] =
  RowEncoder.encoder[(YearMonth, MonthReport.Occurrence), YearMonth, Seq[FreqBucket], Long, Option[LocalDate], Option[CapiId]](0, 1, 2, 3, 4) {
    r => (r._1, r._2.freqByDay, r._2.count, r._2.firstSeenDuringRun.map(_ => r._1.onDayOfMonth(r._2.dayOfFirstUseInMonth)), r._2.firstSeenDuringRun)
  }