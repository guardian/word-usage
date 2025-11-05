package com.gu.words.store


import cats.effect.*
import com.gu.words.addExtension
import kantan.csv.*
import kantan.csv.ops.*

import java.io.BufferedInputStream
import java.nio.file.Files.{newInputStream, newOutputStream}
import java.nio.file.StandardCopyOption.REPLACE_EXISTING
import java.nio.file.{Files, Path}
import java.util.zip.{GZIPInputStream, GZIPOutputStream}
import scala.util.Random

class FileStore extends Store {
  override def write[B: HeaderEncoder](path: Path, rows: IterableOnce[B]): IO[Unit] =
    IO.blocking(Files.createDirectories(path.getParent)) >> {
      val tmpPath = path.addExtension(s"${Random.alphanumeric.take(5).mkString}.tmp")
      Resource.fromAutoCloseable(IO.blocking(new GZIPOutputStream(newOutputStream(tmpPath)))).use {
        outputStream => IO.blocking(outputStream.writeCsv(rows, rfc))
      } >> IO.blocking(Files.move(tmpPath, path, REPLACE_EXISTING))
    }

  override def read[B: HeaderDecoder](path: Path): IO[Seq[B]] = {
    val punk = 8 * 1024
    Resource.fromAutoCloseable(IO.blocking(new BufferedInputStream(new GZIPInputStream(newInputStream(path), punk), 4 * punk))).use {
      outputStream => IO.blocking(outputStream.readCsv[Seq, B](rfc).flatMap(_.toOption))
    }
  }
}
