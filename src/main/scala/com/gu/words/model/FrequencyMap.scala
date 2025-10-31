package com.gu.words.model

type FrequencyMap[T] = Map[T, Int]

extension [T](freqMap: FrequencyMap[T])
  def add(other: FrequencyMap[T]): FrequencyMap[T] = (for {
    key <- freqMap.keySet ++ other.keySet
  } yield key -> (freqMap.getOrElse(key, 0) + other.getOrElse(key, 0))).toMap
