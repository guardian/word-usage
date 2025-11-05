package com.gu.words.store

import cats.effect.IO
import kantan.csv.{HeaderDecoder, HeaderEncoder}

import java.nio.file.Path

trait Store {
  def write[B: HeaderEncoder](path: Path, rows: IterableOnce[B]): IO[Unit]

  def read[B: HeaderDecoder](path: Path): IO[Seq[B]]
}
