package com.gu.words.model

import java.time.Period.ofMonths
import java.time.{LocalDate, Month}
import scala.jdk.StreamConverters.*
import scala.util.Try

case class YearMonth(year: Int, month: Month) {
  lazy val firstDayOfMonth: LocalDate = onDayOfMonth(1)

  lazy val daysOfMonth: Seq[LocalDate] = firstDayOfMonth.datesUntil(firstDayOfMonth.plusMonths(1)).toScala(Seq)

  def to(other: YearMonth): Seq[YearMonth] =
    firstDayOfMonth.datesUntil(other.firstDayOfMonth.plusMonths(1), ofMonths(1)).toScala(Seq).map(_.yearMonth)

  def onDayOfMonth(dayOfMonth: Int): LocalDate = LocalDate.of(year, month, dayOfMonth)

  override lazy val toString: String = firstDayOfMonth.toString.dropRight(3)
}

object YearMonth {
  given Ordering[YearMonth] = Ordering.by(ym => (ym.year, ym.month))

  def parse(text: String): Option[YearMonth] = Try(LocalDate.parse(s"$text-01")).toOption.map(_.yearMonth)
}

extension (d: LocalDate)
  def yearMonth: YearMonth = YearMonth(d.getYear, d.getMonth)
