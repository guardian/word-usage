package com.gu.words.capi

import cats.effect.std.Semaphore
import cats.effect.{FiberIO, IO}

import scala.concurrent.duration.FiniteDuration

final class TokenBucket private (
  sem: Semaphore[IO],
  capacity: Long,
  refillAmount: Long,
  refillPeriod: FiniteDuration
) {
  def take: IO[Unit] = sem.acquire

  private def refillLoop: IO[Unit] =
    IO.sleep(refillPeriod) >>
      sem.releaseN(refillAmount.min(capacity)) >>
      refillLoop

  private def start: IO[FiberIO[Unit]] = refillLoop.start
}

object TokenBucket {
  def create(
    capacity: Int,
    refillAmount: Int,
    refillPeriod: FiniteDuration
  ): IO[TokenBucket] = for {
    sem <- Semaphore[IO](capacity.toLong)
    tb = new TokenBucket(sem, capacity, refillAmount, refillPeriod)
    _ <- tb.start
  } yield tb
}