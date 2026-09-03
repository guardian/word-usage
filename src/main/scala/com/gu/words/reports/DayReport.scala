package com.gu.words.reports

import com.gu.contentapi.client.model.CapiId
import com.gu.words.*
import com.gu.words.model.ContentSummary

import scala.collection.immutable.SortedMap

object DayReport {
  case class Occurrence(freq: Int, firstInstance: CapiId)

  def dayReportOn(contentSummaries: Seq[ContentSummary]): DayReport = {
    {
      val summariesByDay = contentSummaries.groupBy(_.day)
      require(summariesByDay.size <= 1)
    }

    DayReport(
      SortedMap.from(for {
        word <- contentSummaries.flatMap(_.words.keySet).toSet
      } yield word -> {
        val tuples: Seq[(ContentSummary, Int)] = for {
          cSum <- contentSummaries
          wordCount <- cSum.words.get(word)
        } yield (cSum, wordCount)

        DayReport.Occurrence(tuples.map(_._2).sum, tuples.map(_._1).minBy(_.firstPublicationDate).capiId)
      })
    )
  }
}

case class DayReport(occurrenceByWord: Map[Word, DayReport.Occurrence])