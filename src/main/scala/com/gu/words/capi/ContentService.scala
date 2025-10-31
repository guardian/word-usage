package com.gu.words.capi

import cats.effect.IO
import cats.syntax.all.*
import com.gu.words.model.{ContentSummary, YearMonth}
import com.gu.words.reports.DayReport

import java.time.LocalDate
import scala.concurrent.Future

class ContentService(client: IOCapiClient) {
  
  def dayReportOn(date: LocalDate): IO[DayReport] = for {
    contentSummaries <-
      client.paginatedStream(ContentSummary.queryFor(date))(_.results).map(ContentSummary.from).unNone.filter(_.day == date).compile.toList
  } yield DayReport.dayReportOn(contentSummaries)

  def fetchDayReportsFor(yearMonth: YearMonth): IO[Map[LocalDate, DayReport]] =
    yearMonth.daysOfMonth.parTraverse(date => dayReportOn(date).map(date -> _)).map(_.toMap)
    
//  Future.traverse(yearMonth.daysOfMonth) { date => dayReportOn(date)
//    
//  }
}
