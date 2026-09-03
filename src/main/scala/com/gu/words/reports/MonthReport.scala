package com.gu.words.reports

import com.gu.contentapi.client.model.CapiId
import com.gu.words.*
import com.gu.words.model.*
import com.gu.words.reports.MonthReport.Occurrence.FreqBucket
import com.gu.words.reports.MonthReport.Occurrence.FreqBucket.{encodedChars, lowerInclusiveValueByIndex}
import kantan.csv.ops.*
import kantan.csv.*

import java.time.LocalDate

case class MonthReport(occurrenceByWord: Map[Word, MonthReport.Occurrence]) {
  lazy val mostCommon: Seq[(Word, MonthReport.Occurrence)] = occurrenceByWord.toSeq.sortBy(_._2.freqByDay.max).reverse.take(10)

  def csvRows: Seq[(Word, MonthReport.Occurrence)] = occurrenceByWord.toSeq.sortBy(_._1)
}

given RowCodec[(Word, MonthReport.Occurrence)] =
  RowCodec.codec[(Word, MonthReport.Occurrence), Seq[FreqBucket], Word, Int, Option[CapiId]](0, 1, 2, 3) {
    (freqByDay, word, count, firstSeenDuringRun) => word -> MonthReport.Occurrence(count, freqByDay, firstSeenDuringRun)
  } {
    (word, mo) => (mo.freqByDay, word, mo.count, mo.firstSeenDuringRun)
  }

object MonthReport {
  
  case class Occurrence(count: Int, freqByDay: Seq[FreqBucket], firstSeenDuringRun: Option[CapiId] = None) {
    lazy val dayOfFirstUseInMonth: Int = 1 + freqByDay.indexWhere(_.lowerInclusive > 0)
  }

  object Occurrence {
    case class FreqBucket(char: Char) {
      private val encodingIndex = encodedChars.indexOf(char)

      val lowerInclusive: Int = lowerInclusiveValueByIndex(encodingIndex)
      val upperExclusive: Int = lowerInclusiveValueByIndex(encodingIndex + 1)

      def contains(value: Int): Boolean = lowerInclusive <= value && value < upperExclusive

      override val toString:String = char.toString
    }

    object FreqBucket {
      given Ordering[FreqBucket] = Ordering.by(_.lowerInclusive)

      val encodedChars: Seq[Char] = ('0' to '9') ++ ('a' to 'z') ++ ('A' to 'Z')

      def lowerInclusiveValueByIndex(index: Int): Int =
        if (index >= 0 && index <= 9) index else (10 * Math.pow(1.2, index - 10)).toInt

      def lowerInclusiveValueOf(char: Char): Int =
        lowerInclusiveValueByIndex(encodedChars.indexOf(char))

      val All: Seq[FreqBucket] = encodedChars.map(FreqBucket(_))

      def forValue(value: Int): FreqBucket = All.find(_.contains(value)).get

      def forChar(c: Char): FreqBucket = All.find(_.char == c).get
    }
  }

  def from(yearMonth: YearMonth, dayReportsByDay: Map[LocalDate, DayReport], previouslySeen: Map[Word, WordUsage]): MonthReport = {
    require(dayReportsByDay.keys.forall(_.yearMonth == yearMonth))
    MonthReport((for {
      word <- dayReportsByDay.values.flatMap(_.occurrenceByWord.keys)
    } yield word -> {
      val wordOccurrenceByDay: Map[LocalDate, DayReport.Occurrence] =
        dayReportsByDay.flatMap((day, dayReport) => dayReport.occurrenceByWord.get(word).map(day -> _))

      MonthReport.Occurrence(
        wordOccurrenceByDay.map(_._2.freq).sum,
        yearMonth.daysOfMonth.map(day => FreqBucket.forValue(wordOccurrenceByDay.get(day).map(_.freq).getOrElse(0))),
        Option.unless(previouslySeen.contains(word))(wordOccurrenceByDay.minBy(_._1)._2.firstInstance)
      )
    }).toMap)
  }

}
