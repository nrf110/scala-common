package com.github.nrf110.util.extensions

import java.util.concurrent.TimeoutException

import akka.actor.ActorSystem

import scala.concurrent.{Promise, Future, ExecutionContext}
import scala.concurrent.duration.FiniteDuration
import scala.util.control.NonFatal
import scala.util.{Failure, Success}
import scala.language.implicitConversions

private[extensions] class FutureExtensions[A](underlying: Future[A]) {
  def fold[B](success: A => B, failure: Throwable => B)(implicit ec: ExecutionContext): Future[B] = {
    val promise = Promise[B]()

    underlying.onComplete {
      case Success(value) => promise.success(success(value))
      case Failure(NonFatal(t)) => promise.failure(t)
      case Failure(t) => throw t
    }

    promise.future
  }
}

private[extensions] class FutureCompanionExtensions(underlying: Future.type) {
  def timeout[A](duration: FiniteDuration, msg: String = "Future Timeout")(implicit system: ActorSystem): Future[A] = {
    import system.dispatcher
    akka.pattern.after[A](duration, system.scheduler)(Future.failed(new TimeoutException(msg)))
  }
}
