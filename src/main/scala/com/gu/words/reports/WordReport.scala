package com.gu.words.reports

import com.gu.words.capi.CapiId
import com.gu.words.reports.WordReport.FirstUse
import com.gu.words.Word
import com.gu.words.model.YearMonth
import kantan.csv.java8.*
import kantan.csv.ops.*
import kantan.csv.*

import java.time.LocalDate
import scala.collection.immutable.SortedMap

case class WordReport(count: Long, firstUse: FirstUse) {
  def add(monthReport: MonthReport.Occurrence): WordReport = copy(count = count + monthReport.count)
}

case object WordReport {
  case class FirstUse(day: LocalDate, capiId: CapiId)

  given RowCodec[(Word, WordReport)] =
    RowCodec.caseCodec[(Word, WordReport), Word, Long, LocalDate, CapiId](0, 1, 2, 3)(
      (word, count, firstUseDay, firstUseCapiId) => word -> WordReport(count, FirstUse(firstUseDay, firstUseCapiId))
    )(r => Some(r._1, r._2.count, r._2.firstUse.day, r._2.firstUse.capiId))

  object FirstUse {
    def from(yearMonth: YearMonth, monthReport: MonthReport.Occurrence): FirstUse = FirstUse(
      yearMonth.onDayOfMonth(monthReport.dayOfFirstUseInMonth),
      monthReport.firstSeenDuringRun.get // I'm so naughty!
    )
  }

  def fromFirstOccurrenceInRun(yearMonth: YearMonth, monthReport: MonthReport.Occurrence) =
    WordReport(monthReport.count, FirstUse.from(yearMonth, monthReport))

  def write[A: CsvSink](sink: A, reportsByWord: Map[Word, WordReport]): Unit =
    sink.writeCsv(SortedMap.from(reportsByWord), rfc)
}
