package com.github.nrf110.util.extensions

import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}
import scala.language.implicitConversions

private[extensions] class TryExtensions[A](underlying: Try[A]) {
  def fold[B](success: A => B, failure: Throwable => B): B = underlying match {
    case Success(value) => success(value)
    case Failure(NonFatal(t)) => failure(t)
    case Failure(t) => throw t
  }
}
