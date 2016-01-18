package com.github.nrf110.io.pool

import com.typesafe.config.Config

sealed trait ResizeResult
object ResizeResult {
  case object NoOp extends ResizeResult
  case class Grow(by: Int) extends ResizeResult
  case class Shrink(by: Int) extends ResizeResult
}

trait PoolResizer {
  def resize(requestsWaiting: Int, itemsAvailable: Int, totalItems: Int): ResizeResult
}

class DefaultPoolResizer(config: Config) extends PoolResizer {
  private[this] val initialSize = config.getInt("initial")
  private[this] val minSize = config.getInt("min")
  private[this] val maxSize = config.getInt("max")
  private[this] val step = config.getInt("step")
  private[this] val backOffLimit: Int = 3
  private[this] var backOffCount: Int = 0

  require(initialSize >= minSize && initialSize <= maxSize)

  def resize(requestsWaiting: Int, itemsAvailable: Int, totalItems: Int): ResizeResult = {
    if (totalItems == 0) {
      ResizeResult.Grow(initialSize)
    } else if (requestsWaiting > itemsAvailable && totalItems < maxSize) {
      backOffCount = 0
      ResizeResult.Grow(math.min(maxSize - totalItems, step))
    } else if ((itemsAvailable - step) > requestsWaiting) {
      if (backOffCount >= backOffLimit) {
        backOffCount = 0
        ResizeResult.Shrink(math.min(totalItems - minSize, step))
      } else {
        backOffCount = backOffCount + 1
        ResizeResult.NoOp
      }
    } else ResizeResult.NoOp
  }
}
