package com.gu.words.capi

case class CapiId(value: String) {
  require(value.nonEmpty, s"Capi id is empty")
  require(!value.startsWith("/"), s"Capi id must not start with a forward slash: $value"
  )
}
