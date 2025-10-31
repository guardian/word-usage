package com.gu.words.reports

import com.gu.words.reports.MonthReport.Occurrence.FreqBucket.lowerInclusiveValueOf
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.{Inspectors, OptionValues}

class MonthReportTest extends AnyFlatSpec with Matchers with Inspectors with OptionValues {
  
  "MonthReport frequency bucketing" should "be decent" in {
    // The most common word - 'the' occurred 15116 times at its peak in April 2025.

    lowerInclusiveValueOf('0') shouldBe 0
    lowerInclusiveValueOf('9') shouldBe 9
    lowerInclusiveValueOf('a') shouldBe 10
    lowerInclusiveValueOf('b') shouldBe 12
    lowerInclusiveValueOf('c') shouldBe 14
    lowerInclusiveValueOf('z') shouldBe 953
    lowerInclusiveValueOf('A') shouldBe 1144
    lowerInclusiveValueOf('Z') shouldBe 109205
  }

}