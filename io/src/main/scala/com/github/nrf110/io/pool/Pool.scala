package com.github.nrf110.io.pool

import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration

trait Pool[A] {
  def reserve(timeout: Option[FiniteDuration] = None): Future[A]
  def release(item: A): Unit
}
