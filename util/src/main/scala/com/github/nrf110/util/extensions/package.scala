package com.github.nrf110.util

import scala.concurrent.Future
import scala.util.Try

package object extensions {
  implicit def try2TryExtensions[A](underlying: Try[A]): TryExtensions[A] =
    new TryExtensions[A](underlying)

  object future {
    implicit def future2FutureExtensions[A](underlying: Future[A]): FutureExtensions[A] =
      new FutureExtensions[A](underlying)

    implicit def futureCompanion2FutureCompanionExtensions(underlying: Future.type): FutureCompanionExtensions =
      new FutureCompanionExtensions(underlying)
  }
}
