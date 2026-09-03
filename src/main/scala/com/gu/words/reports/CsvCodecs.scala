package com.gu.words.reports

import com.gu.contentapi.client.model.CapiId
import com.gu.words.*
import com.gu.words.model.YearMonth
import com.gu.words.reports.MonthReport.Occurrence.FreqBucket
import kantan.csv.*

given CellCodec[CapiId] = CellCodec.from(s => Right(CapiId(s)), _.value)
given CellCodec[Seq[FreqBucket]] = CellCodec.from(s => Right(s.map(c => FreqBucket.forChar(c))), _.mkString)
given CellEncoder[YearMonth] = CellEncoder.from(_.toString)

