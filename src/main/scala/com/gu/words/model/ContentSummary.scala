package com.gu.words.model

import com.gu.contentapi.client.ContentApiClient
import com.gu.contentapi.client.model.{CapiId, SearchQuery}
import com.gu.contentapi.client.model.v1.Content
import com.gu.words.Word
import com.madgag.scala.collection.decorators.*
import opennlp.tools.tokenize.{SimpleTokenizer, Tokenizer}

import java.time.ZoneOffset.UTC
import java.time.{Instant, LocalDate}

object ContentSummary {
  val tokenizer: Tokenizer = SimpleTokenizer.INSTANCE

  def freqMap(text: String): FrequencyMap[Word] = {
    val tokens = tokenizer.tokenize(text).filter(!_.matches("""^[\p{Punct}\d]+$""")).filter(w => w.length > 1 && w.length < 50).map(_.toLowerCase().intern())
    tokens.toSeq.groupUp(identity)(_.length)
  }

  def from(content: Content): Option[ContentSummary] = for {
    fields <- content.fields
    firstPublished <- fields.firstPublicationDate
    bodyText <- fields.bodyText
  } yield ContentSummary(CapiId(content.id), Instant.ofEpochMilli(firstPublished.dateTime), freqMap(bodyText))


  val baseQuery: SearchQuery = ContentApiClient.search
    .orderBy("newest").useDate("first-publication")
    .showFields("firstPublicationDate,bodyText")
    .tag("-info/info,-tone/sponsoredfeatures,-type/crossword,-extra/extra,-tone/advertisement-features") // copied from https://github.com/guardian/frontend/blob/9a69dd4fdc2f09cbc2459dedd933b03215d2cad7/applications/app/services/NewsSiteMap.scala#L65
    .pageSize(100) // https://github.com/guardian/content-api/pull/836

  def queryFor(localDate: LocalDate): SearchQuery =
    baseQuery.fromDate(localDate.atStartOfDay(UTC).toInstant).toDate(localDate.plusDays(1).atStartOfDay(UTC).toInstant)
}

case class ContentSummary(capiId: CapiId, firstPublicationDate: Instant, words: FrequencyMap[Word]) {
  val day: LocalDate = LocalDate.ofInstant(firstPublicationDate, UTC)
}