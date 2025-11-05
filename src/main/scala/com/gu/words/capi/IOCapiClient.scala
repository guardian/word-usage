package com.gu.words.capi

import cats.effect.IO
import com.gu.contentapi.client.model.Direction.Next
import com.gu.contentapi.client.model.{ContentApiQuery, PaginatedApiQuery}
import com.gu.contentapi.client.{ContentApiClient, Decoder, GuardianContentClient}
import com.gu.words.{logIfSlow, logTime}
import com.twitter.scrooge.ThriftStruct
import fs2.Chunk
import fs2.Stream.unfoldChunkLoopEval
import retry.*
import retry.ResultHandler.*
import retry.RetryPolicies.*

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.*
import sttp.client4.*

trait IOCapiClient {
  def getResponse[Resp <: ThriftStruct : Decoder](query: ContentApiQuery[Resp]): IO[Resp]

  def paginatedStream[R <: ThriftStruct : Decoder, E, T](query: PaginatedApiQuery[R, E])(f: R => scala.collection.Seq[T]): fs2.Stream[IO, T] =
    unfoldChunkLoopEval(query)(getResponse(_).map {
      resp => (Chunk.from(f(resp).toList), query.followingQueryGiven(resp, Next))
    })
}

object IOCapiClient {

  private val backend = DefaultSyncBackend()
  val capiUrl = uri"https://content.guardianapis.com/"

  val retryPolicy: RetryPolicy[IO, Throwable] = limitRetriesByCumulativeDelay(60.seconds, fullJitter(1.seconds))
  val retryLogging: ErrorHandler[IO, Nothing] = retryOnAllErrors((ex, _) => IO.println(s"Retrying $ex"))

  def from(apiKey: String)(using ExecutionContext): IO[IOCapiClient] = for {
    rateLimiter <- createRateLimiterAppropriateTo(apiKey)
  } yield new IOCapiClient {
    val client = new GuardianContentClient(apiKey)

    override def getResponse[Resp <: ThriftStruct : Decoder](query: ContentApiQuery[Resp]): IO[Resp] =
      retryingOnErrors(execute(query))(retryPolicy, retryLogging)

    private def execute[Resp <: ThriftStruct : Decoder](query: ContentApiQuery[Resp]) = {
      rateLimiter.enforceWithDelay.logIfSlow("Throttling CAPI call with artificial delay")
        >> IO.fromFuture(IO(client.getResponse(query)))
    }
  }

  private def createRateLimiterAppropriateTo(apiKey: String): IO[TokenBucket] = {
    val minuteQuota = getPerMinuteQuotaFor(apiKey)
    IO.println(s"CAPI Quota per min: $minuteQuota") >>
      TokenBucket.create(minuteQuota, minuteQuota/60, 1.second)
  }

  private def getPerMinuteQuotaFor(apiKey: String) =
    basicRequest.get(capiUrl.addParam("api-key", apiKey)).send(backend).header("x-ratelimit-limit-minute").get.toInt
}